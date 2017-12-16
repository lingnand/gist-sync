{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A module to facilitate the syncing of Gist files
module Network.GitHub.Gist.Sync
  ( SyncFile(..)
  , LocalFileExist(..)
  , LocalFileInfo
  , SyncAction(..)
  , SyncPlan
  , SyncConflict(..)
  , PathMapper
  , GistFileId
  , planFilePath
  , pathMapper
  , localToGistFile
  , gistToLocalFile

  , computeSyncActions

  , module Network.GitHub.Types.Gist
  ) where

import           Control.Applicative ((<|>))
import           Control.Lens
import qualified Crypto.Hash as H
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map as Map
import           Data.Maybe
import           Data.String
import qualified Data.Text as T
import           Data.Time.Clock
import qualified Filesystem.Path.CurrentOS as P
import           Network.GitHub.Types.Gist

type GistFileId = (GistId, FileId)

-- | The sync status for a specific file
data SyncFile a = SyncFile
  { syncFilePath   :: P.FilePath
  , syncGistFileId :: GistFileId
  , syncFileHash   :: H.Digest a -- ^ Hash of the file when it was synced
  , syncFileTime   :: UTCTime    -- ^ The time of the sync
  } deriving (Show, Eq)

data LocalFileExist a = LocalFileExist
  { localFileHash         :: H.Digest a
    -- ^ Hash of the file as of a moment in time
  , localFileLastModified :: UTCTime
    -- ^ Last modified time of the file
  } deriving (Show, Eq, Ord)

-- | Just LocalFileExist for existing info, or Nothing if file disappeared
type LocalFileInfo a = Maybe (LocalFileExist a)

type SyncPlan a = Either (SyncConflict a) (SyncAction a)

planFilePath :: SyncPlan a -> P.FilePath
planFilePath = either conflictLocalFilePath localFilePath

data SyncAction a = UpdateLocal  { localFilePath    :: P.FilePath
                                 , remoteGistFileId :: GistFileId
                                 , remoteFileURL    :: T.Text
                                 }
                  | CreateLocal  { localFilePath    :: P.FilePath
                                 , remoteGistFileId :: GistFileId
                                 , remoteFileURL    :: T.Text
                                 }
                  | DeleteLocal  { localFilePath    :: P.FilePath
                                 , remoteFileId     :: FileId
                                 }
                  | UpdateRemote { localFilePath    :: P.FilePath
                                 , localFileInfo    :: LocalFileExist a
                                 , remoteGistFileId :: GistFileId
                                 }
                  | CreateRemote { localFilePath    :: P.FilePath
                                 , localFileInfo    :: LocalFileExist a
                                 , remoteFileId     :: FileId
                                 }
                  | DeleteRemote { localFilePath    :: P.FilePath
                                 , remoteGistFileId :: GistFileId
                                 }
                  deriving (Show, Eq, Ord)

data SyncConflict a = SyncConflict
  { conflictLocalFilePath    :: P.FilePath
  , conflictLocalFileInfo    :: LocalFileExist a
  , conflictRemoteGistFileId :: GistFileId
  , conflictRemoteFileURL    :: T.Text
  , conflictRemoteUpdateTime :: UTCTime
  } deriving (Show, Eq, Ord)

-- | A isomorphism between local file path and gist file id
newtype PathMapper = PathMapper { unPathMapper :: Iso' P.FilePath FileId }

pathMapper :: (P.FilePath -> FileId) -> (FileId -> P.FilePath) -> PathMapper
pathMapper t f = PathMapper $ iso t f

localToGistFile :: PathMapper -> P.FilePath -> FileId
localToGistFile (PathMapper is) = view is

gistToLocalFile :: PathMapper -> FileId -> P.FilePath
gistToLocalFile (PathMapper is) = review is


-- Syncing strategy
-- 1. check if there is any update from the cloud since the time when we last sync
-- 2. check if there is any change on the file since the time when we last sync
--    we can do this by a) checking last accessed time of the file b) saving a md5sum of the file when we last synced
-- 3. if both 1 and 2 have changed, we need to make a decision which side to use
-- 4. otherwise, update the end which is in need of an update
computeSyncActions
  :: forall a e. IsString e
  => PathMapper
  -> [(Either P.FilePath (SyncFile a), LocalFileInfo a)]
                    -- ^ A list of files, either synced in the past,
                    --   or potentially need to be synced fresh, all with current info
  -> [Gist]    -- ^ All gists from the remote end
  -> [Either e (SyncPlan a)] -- ^ A list of actions with their resultant sync files
computeSyncActions pMapper files gists = mapMaybe (uncurry onPair) (Map.toList pairs)
  where
    altLocal Nothing x = x
    altLocal x Nothing = x
    altLocal (Just (Left _,_)) x@(Just (Right _,_)) = x
    altLocal x _ = x
    pairs :: Map.Map ( P.FilePath, FileId )
      ( Maybe (Either P.FilePath (SyncFile a), LocalFileInfo a)
      , Maybe (File, Gist) )
    pairs = Map.fromListWith (\(x,y) (x',y') -> (altLocal x x', y <|> y')) $
      [ ((p, localToGistFile pMapper p), (Just f, Nothing))
      | f@(eth,_) <- files
      , let p = either id syncFilePath eth
      ]
      ++
      [ ((gistToLocalFile pMapper fid, fid), (Nothing, Just (f, g)))
      | g <- gists
      , (fid, f) <- HMap.toList $ gistFiles g
      ]
    onPair
      :: ( P.FilePath, FileId )
      -> ( Maybe (Either P.FilePath (SyncFile a), LocalFileInfo a)
         , Maybe (File, Gist) )
      -> Maybe (Either e (SyncPlan a))
    onPair (p,fid) (Nothing, Just (gfile, gist))
    -- never synced to local in the past
      = Just . Right . Right $ CreateLocal
      { localFilePath = p
      , remoteFileURL = fileRawUrl gfile
      , remoteGistFileId = (gistId gist, fid)
      }
    onPair (p,fid) (Just (Left _, linfo), Nothing)
      -- never synced to remote in the past
      | Just local <- linfo
      = Just . Right . Right $ CreateRemote
      { localFilePath = p
      , localFileInfo = local
      , remoteFileId = fid
      }
      | otherwise
      = Just . Left . fromString $
      "filepath "++show p++" not synced in the past, but no LocalFileInfo found!"
    onPair (p,fid) (Just (Right _, local), Nothing)
      -- remote deletion..?
      | Just _ <- local
      = Just . Right . Right $ DeleteLocal
      { localFilePath = p
      , remoteFileId = fid
      }
      | otherwise -- deleted in both ends..?
      = Nothing
    onPair (p,fid) (Just (Right sync, Just local), Just (gfile, gist))
      | remoteChanged, not localChanged = Just . Right . Right $ UpdateLocal
        { localFilePath = p
        , remoteGistFileId = gistFileId
        , remoteFileURL = fileRawUrl gfile
        }
      | not remoteChanged, localChanged = Just . Right . Right $ UpdateRemote
        { localFilePath = p
        , localFileInfo = local
        , remoteGistFileId = gistFileId
        }
      | not remoteChanged, not localChanged = Nothing
      where localChanged = localFileHash local /= syncFileHash sync
            remoteChanged = gistUpdatedAt gist > syncFileTime sync
            gistFileId = (gistId gist, fid)
    onPair (p,fid) (Just (Right _, Nothing), Just (_, gist))
      = Just . Right . Right $ DeleteRemote
      { localFilePath = p
      , remoteGistFileId = (gistId gist, fid)
      }
    onPair (p,fid) (Just (_, Just local), Just (gfile, gist))
      -- both have changed, OR
      -- never synced, but there is a remote correspondence
      = Just . Right . Left $ SyncConflict
      { conflictLocalFilePath = p
      , conflictLocalFileInfo = local
      , conflictRemoteGistFileId = (gistId gist, fid)
      , conflictRemoteFileURL = fileRawUrl gfile
      , conflictRemoteUpdateTime = gistUpdatedAt gist
      }
    onPair _ _ = Nothing
