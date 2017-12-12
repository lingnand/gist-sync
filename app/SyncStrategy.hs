{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
-- | Module representing specific strategies to filter sync actions
module SyncStrategy
  (
    SyncStrategy
  , applyStrategy
  , applyStrategyToList

  , (<||>)
  , no

  , customRewrite
  , customFilter

  , acceptAll
  , dropAll
  , localOnly
  , remoteOnly
  , createLocal
  , createRemote
  , createAny
  , updateLocal
  , updateRemote
  , updateAny
  , deleteLocal
  , deleteRemote
  , deleteAny
  , ignoreConflicts

  , oneOfFiles
  , filenameMatches

  , useRemoteForConflict
  , useLocalForConflict
  , useNewerForConflict

  -- parsers
  , strategyP
  , parseStrategy
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Either
import           Data.Maybe
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as P
import qualified Network.GitHub.Gist.Sync as S
import qualified Text.Parsec as E
import qualified Text.Parsec.String as E
import qualified Text.Regex.TDFA as R

newtype SyncStrategy = SyncStrategy
  { unSyncStrategy :: forall a. S.SyncPlan a -> Maybe (S.SyncPlan a) }

-- | A Monoid instance where strategies are chained together so the first
--   continues on the result of the second
--   this is similar to Endo
instance Monoid SyncStrategy where
  mempty = SyncStrategy pure
  SyncStrategy s1 `mappend` SyncStrategy s2 = SyncStrategy $ s1 <=< s2

-- | A more intuitive operator
(<.>) :: SyncStrategy -> SyncStrategy -> SyncStrategy
(<.>) = mappend

-- | Apply a strategy on a single plan
applyStrategy :: SyncStrategy -> S.SyncPlan a -> Maybe (S.SyncPlan a)
applyStrategy = unSyncStrategy

-- | Apply a strategy on a set of actions to filter only valid ones
applyStrategyToList :: SyncStrategy -> [S.SyncPlan a] -> [S.SyncPlan a]
applyStrategyToList = mapMaybe . unSyncStrategy

---- Combinators on the strategy
-- | Combine two strategies such that for each incoming action, if first
--   strategy yields no response, the second strategy will handle it
infixl 2 <||>
(<||>) :: SyncStrategy -> SyncStrategy -> SyncStrategy
SyncStrategy x <||> SyncStrategy y = SyncStrategy $ (<|>) <$> x <*> y

no :: SyncStrategy -> SyncStrategy
no (SyncStrategy x) = SyncStrategy $ \a -> case x a of
  Nothing -> Just a
  Just _ -> Nothing

customRewrite :: (forall a. S.SyncPlan a -> Maybe (S.SyncPlan a)) -> SyncStrategy
customRewrite = SyncStrategy

customFilter :: (forall a. S.SyncPlan a -> Bool) -> SyncStrategy
customFilter f = SyncStrategy $ \x -> guard (f x) >> pure x

---- Filters
acceptAll :: SyncStrategy
acceptAll = mempty

dropAll :: SyncStrategy
dropAll = SyncStrategy $ const Nothing

localOnly :: SyncStrategy
localOnly = customFilter f
  where f (Right S.UpdateLocal{}) = True
        f (Right S.CreateLocal{}) = True
        -- XXX: should we auto override local change to match remote?
        f (Left S.SyncConflict{}) = True
        f _ = False

remoteOnly :: SyncStrategy
remoteOnly = customFilter f
  where f (Right S.UpdateRemote{}) = True
        f (Right S.CreateRemote{}) = True
        -- XXX: should we auto override remote change to match local?
        f (Left S.SyncConflict{}) = True
        f _ = False

createLocal :: SyncStrategy
createLocal = customFilter f
  where f (Right S.CreateLocal{}) = True
        f _ = False

createRemote :: SyncStrategy
createRemote = customFilter f
  where f (Right S.CreateRemote{}) = True
        f _ = False

createAny :: SyncStrategy
createAny = createLocal <||> createRemote

updateLocal :: SyncStrategy
updateLocal = customFilter f
  where f (Right S.UpdateLocal{}) = True
        f _ = False

updateRemote :: SyncStrategy
updateRemote = customFilter f
  where f (Right S.UpdateRemote{}) = True
        f _ = False

updateAny :: SyncStrategy
updateAny = updateLocal <||> updateRemote


deleteLocal :: SyncStrategy
deleteLocal = customFilter f
  where f (Right S.DeleteLocal{}) = True
        f _ = False

deleteRemote :: SyncStrategy
deleteRemote = customFilter f
  where f (Right S.DeleteRemote{}) = True
        f _ = False

deleteAny :: SyncStrategy
deleteAny = deleteLocal <||> deleteRemote

ignoreConflicts :: SyncStrategy
ignoreConflicts = SyncStrategy $ mfilter isRight . pure

oneOfFiles
  :: [P.FilePath]
  -> SyncStrategy
oneOfFiles files = customFilter f
  where f p = S.planFilePath p `elem` files

filenameMatches :: R.Regex -> SyncStrategy
filenameMatches regex = customFilter $ \x ->
  R.match regex (P.encodeString . P.filename $ S.planFilePath x)

---- Conflict resolvers
useRemoteForConflict :: SyncStrategy
useRemoteForConflict = SyncStrategy mapper
  where mapper (Left S.SyncConflict{..})
          = Just $ Right S.UpdateLocal
            { localFilePath = conflictLocalFilePath
            , remoteFileURL = conflictRemoteFileURL
            , remoteGistFileId = conflictRemoteGistFileId
            }
        mapper x
          = Just x

useLocalForConflict :: SyncStrategy
useLocalForConflict = SyncStrategy mapper
  where mapper (Left S.SyncConflict{..})
          = Just $ Right S.UpdateRemote
            { localFilePath = conflictLocalFilePath
            , localFileInfo = conflictLocalFileInfo
            , remoteGistFileId = conflictRemoteGistFileId
            }
        mapper x
          = Just x

useNewerForConflict :: SyncStrategy
useNewerForConflict = SyncStrategy mapper
  where mapper x@(Left S.SyncConflict{..})
          | S.localFileLastModified conflictLocalFileInfo > conflictRemoteUpdateTime
          = unSyncStrategy useLocalForConflict x
          | otherwise
          = unSyncStrategy useRemoteForConflict x
        mapper x
          = Just x

-- parser for SyncStrategy
syncStrategyTable :: [(String, SyncStrategy)]
syncStrategyTable =
  [ ("acceptAll", acceptAll)
  , ("dropAll", dropAll)
  , ("localOnly", localOnly)
  , ("remoteOnly", remoteOnly)
  , ("createLocal", createLocal)
  , ("createRemote", createRemote)
  , ("createAny", createAny)
  , ("updateLocal", updateLocal)
  , ("updateRemote", updateRemote)
  , ("updateAny", updateAny)
  , ("deleteLocal", deleteLocal)
  , ("deleteRemote", deleteRemote)
  , ("deleteAny", deleteAny)
  , ("ignoreConflicts", ignoreConflicts)
  , ("useRemoteForConflict", useRemoteForConflict)
  , ("useLocalForConflict", useLocalForConflict)
  , ("useNewerForConflict", useNewerForConflict)
  ]

byNameP :: E.Parser SyncStrategy
byNameP = E.choice $ f <$> syncStrategyTable
  where f (name, val) = val <$ E.try (E.string name)

-- XXX: very crude hand parsing of list
oneOfFilesP :: E.Parser SyncStrategy
oneOfFilesP = oneOfFiles <$ E.string "oneOfFiles" <* E.skipMany1 E.space
          <*> (E.char '['
                *> ((:) <$> (E.spaces *> file)
                        <*  E.spaces
                        <*> many (E.char ',' *> E.spaces *> file <* E.spaces))
                <*
                E.char ']')
  where file = P.fromText . T.pack <$> E.many (E.noneOf " ,]")


filenameMatchesP :: E.Parser SyncStrategy
filenameMatchesP = filenameMatches <$ E.string "filenameMatches" <* E.skipMany1 E.space
                   <*> (E.char '/'
                        *> (R.makeRegex <$> E.many1 (E.noneOf "/")) <*
                        E.char '/')

aStrategyP :: E.Parser SyncStrategy
aStrategyP = byNameP <|> oneOfFilesP <|> filenameMatchesP

binaryOpP :: E.Parser (SyncStrategy -> SyncStrategy -> SyncStrategy)
binaryOpP = (<.>) <$ E.string "."
            <|> (<||>) <$ E.string "||"

unaryOpP :: E.Parser (SyncStrategy -> SyncStrategy)
unaryOpP = no <$ E.string "!"

strategyP :: E.Parser SyncStrategy
strategyP =
       unaryOpP <* E.spaces <*> strategyP
   <|> E.char '(' *> E.spaces *> strategyP <* E.spaces <* E.char ')'
   <|> E.try (flip ($) <$> aStrategyP <* E.spaces <*> binaryOpP <* E.spaces <*> strategyP)
   <|> aStrategyP

parseStrategy :: String -> Either E.ParseError SyncStrategy
parseStrategy = E.runParser strategyP () "textual"
