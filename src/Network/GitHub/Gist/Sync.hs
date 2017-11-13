{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A module to facilitate the syncing of Gist files
module Network.GitHub.Gist.Sync
  ( SyncFile(..)
  , LocalFileInfo(..)
  , SyncAction(..)
  , PathMapper
  , computeSyncActions
  ) where

import           Control.Applicative ((<|>))
import           Control.Lens
import qualified Crypto.Hash as H
import qualified Data.HashMap.Strict as HMap
import           Data.Maybe
-- import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Time.Clock
import qualified Filesystem.Path as P
import qualified Network.GitHub.Types.Gist as Gist

type GistFileId = (Gist.GistId, Gist.FileId)

-- | The sync status for a specific file
data SyncFile a = SyncFile
  { syncFilePath   :: P.FilePath
  , syncGistFileId :: GistFileId
  , syncFileHash   :: H.Digest a -- ^ Hash of the file when it was synced
  , syncFileTime   :: UTCTime    -- ^ The time of the sync
  }

data LocalFileInfo a = LocalFileInfo
  { localFileHash         :: H.Digest a -- ^ Hash of the file as of a moment in time
  , localFileLastModified :: UTCTime    -- ^ Last modified time of the file
  }

data SyncAction a = UpdateLocal  { localFilePath    :: P.FilePath
                                 , remoteFileURL    :: T.Text
                                 }
                  | CreateLocal  { localFilePath    :: P.FilePath
                                 , remoteFileURL    :: T.Text
                                 }
                  | UpdateRemote { localFilePath    :: P.FilePath
                                 , localFileInfo    :: LocalFileInfo a
                                 , remoteGistFileId :: GistFileId
                                 }
                  | CreateRemote { localFilePath    :: P.FilePath
                                 , localFileInfo    :: LocalFileInfo a
                                 , remoteFileId     :: Gist.FileId
                                 }
                  | SyncConflict { localFilePath    :: P.FilePath
                                 , localFileInfo    :: LocalFileInfo a
                                 , remoteGistFileId :: GistFileId
                                 , remoteFileURL    :: T.Text
                                 , remoteUpdateTime :: UTCTime
                                 }

-- | A isomorphism between local file path and gist file id
type PathMapper = Prism' P.FilePath Gist.FileId

-- Syncing strategy
-- 1. check if there is any update from the cloud since the time when we last sync
-- 2. check if there is any change on the file since the time when we last sync
--    we can do this by a) checking last accessed time of the file b) saving a md5sum of the file when we last synced
-- 3. if both 1 and 2 have changed, we need to make a decision which side to use
-- 4. otherwise, update the end which is in need of an update
computeSyncActions
  :: forall a.
     PathMapper
  -> [(Either P.FilePath (SyncFile a), LocalFileInfo a)]
                    -- ^ A list of files, either synced in the past,
                    --   or never synced yet, with the current info
  -> [Gist.Gist]    -- ^ *Updated* gists from the remote end
  -> [SyncAction a] -- ^ A list of actions with their resultant sync files
computeSyncActions pMapper files gists = mapMaybe (uncurry onPair) (Map.toList pairs)
  where
    altLocal Nothing x = x
    altLocal x Nothing = x
    altLocal (Just (Left _,_)) x@(Just (Right _,_)) = x
    altLocal x _ = x
    pairs :: Map.Map ( P.FilePath, Gist.FileId )
      ( Maybe (Either P.FilePath (SyncFile a), LocalFileInfo a)
      , Maybe (Gist.File, Gist.Gist) )
    pairs = Map.fromListWith (\(x,y) (x',y') -> (altLocal x x', y <|> y')) $
      [ ((p, view pMapper p), (Just f, Nothing))
      | f@(eth,_) <- files
      , let p = either id syncFilePath eth
      ]
      ++
      [ ((review pMapper fid, fid), (Nothing, Just (f, g)))
      | g <- gists
      , (fid, f) <- HMap.toList $ Gist.files g
      ]
    onPair
      :: ( P.FilePath, Gist.FileId )
      -> ( Maybe (Either P.FilePath (SyncFile a), LocalFileInfo a)
         , Maybe (Gist.File, Gist.Gist) )
      -> Maybe (SyncAction a)
    onPair (p,_) (Nothing, Just (gfile, _))
    -- never synced to local in the past
      = Just CreateLocal
      { localFilePath = p
      , remoteFileURL = Gist.fileRawUrl gfile
      }
    onPair (p,fid) (Just (Left _, local), Nothing)
    -- never synced to remote in the past
      = Just CreateRemote
      { localFilePath = p
      , localFileInfo = local
      , remoteFileId = fid
      }
    onPair (p,_) (Just (Right sync, local), Nothing)
    -- synced in the past
      | localFileHash local /= syncFileHash sync
      -- something changed
      = Just UpdateRemote
      { localFilePath = p
      , localFileInfo = local
      , remoteGistFileId = syncGistFileId sync
      }
      | otherwise
      -- nothing changed
      = Nothing
    onPair (p,fid) (Just (_, local), Just (gfile, gist))
      = Just SyncConflict
      { localFilePath = p
      , localFileInfo = local
      , remoteGistFileId = (Gist.id gist, fid)
      , remoteFileURL = Gist.fileRawUrl gfile
      , remoteUpdateTime = Gist.updated_at gist
      }
    onPair _ _ = Nothing
