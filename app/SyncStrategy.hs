{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
-- | Module representing specific strategies to filter sync actions
module SyncStrategy
  (
    SyncStrategy
  , applyStrategy

  , (<||>)
  , (<->>)
  , no

  , customRewrite
  , customFilter

  , anyFromRemote
  , createLocal
  , createRemote
  , createAny
  , updateLocal
  , updateRemote
  , updateAny

  , oneOfFiles
  , filenameMatches

  , useRemoteForConflict
  , useLocalForConflict
  , useNewerForConflict
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import qualified Filesystem.Path.CurrentOS as P
import qualified Network.GitHub.Gist.Sync as S
import qualified Text.Regex.TDFA as R

newtype SyncStrategy = SyncStrategy
  { unSyncStrategy :: forall a. S.SyncAction a -> Maybe (S.SyncAction a) }

-- | Apply a strategy on a set of actions to filter only valid ones
applyStrategy :: SyncStrategy -> [S.SyncAction a] -> [S.SyncAction a]
applyStrategy = mapMaybe . unSyncStrategy

---- Combinators on the strategy
-- | Combine two strategies such that for each incoming action, if first
--   strategy yields no response, the second strategy will handle it
infixl 2 <||>
(<||>) :: SyncStrategy -> SyncStrategy -> SyncStrategy
SyncStrategy x <||> SyncStrategy y = SyncStrategy $ (<|>) <$> x <*> y

-- | Chain two strategies together so the second continues on the result of the
--   first
infixl 3 <->>
(<->>) :: SyncStrategy -> SyncStrategy -> SyncStrategy
SyncStrategy x <->> SyncStrategy y = SyncStrategy $ x >=> y

no :: SyncStrategy -> SyncStrategy
no (SyncStrategy x) = SyncStrategy $ \a -> case x a of
  Nothing -> Just a
  Just _ -> Nothing

customRewrite :: (forall a. S.SyncAction a -> Maybe (S.SyncAction a)) -> SyncStrategy
customRewrite = SyncStrategy

customFilter :: (forall a. S.SyncAction a -> Bool) -> SyncStrategy
customFilter f = SyncStrategy $ \x -> guard (f x) >> pure x

---- Filters
anyFromRemote :: SyncStrategy
anyFromRemote = customFilter f
  where f S.UpdateLocal{} = True
        f S.CreateLocal{} = True
        f S.SyncConflict{} = True
        f _ = False

createLocal :: SyncStrategy
createLocal = customFilter f
  where f S.CreateLocal{} = True
        f _ = False

createRemote :: SyncStrategy
createRemote = customFilter f
  where f S.CreateRemote{} = True
        f _ = False

createAny :: SyncStrategy
createAny = createLocal <||> createRemote

updateLocal :: SyncStrategy
updateLocal = customFilter f
  where f S.UpdateLocal{} = True
        f _ = False

updateRemote :: SyncStrategy
updateRemote = customFilter f
  where f S.UpdateRemote{} = True
        f _ = False

updateAny :: SyncStrategy
updateAny = updateLocal <||> updateRemote

oneOfFiles
  :: [P.FilePath]
  -> SyncStrategy
oneOfFiles files = customFilter f
  where f act = S.localFilePath act `elem` files

filenameMatches :: R.Regex -> SyncStrategy
filenameMatches regex = customFilter $ \x ->
  R.match regex (P.encodeString . P.filename $ S.localFilePath x)

---- Conflict resolvers
useRemoteForConflict :: SyncStrategy
useRemoteForConflict = SyncStrategy mapper
  where mapper S.SyncConflict{ S.localFilePath, S.remoteFileURL }
          = Just S.UpdateLocal{..}
        mapper x
          = Just x

useLocalForConflict :: SyncStrategy
useLocalForConflict = SyncStrategy mapper
  where mapper S.SyncConflict{ S.localFilePath, S.remoteGistFileId, S.localFileInfo }
          = Just S.UpdateRemote{..}
        mapper x
          = Just x

useNewerForConflict :: SyncStrategy
useNewerForConflict = SyncStrategy mapper
  where mapper x@S.SyncConflict { S.remoteUpdateTime , S.localFileInfo }
          | S.localFileLastModified localFileInfo > remoteUpdateTime
          = unSyncStrategy useLocalForConflict x
          | otherwise
          = unSyncStrategy useRemoteForConflict x
        mapper x
          = Just x
