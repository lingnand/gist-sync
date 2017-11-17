{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A module to facilitate the syncing of Gist files
module Network.GitHub.Gist.Sync
  ( SyncFile(..)
  , LocalFileInfo(..)
  , SyncAction(..)
  , PathMapper
  , GistFileId
  , pathMapper
  , localToGistFile
  , gistToLocalFile

  , computeSyncActions
  ) where

import           Control.Applicative ((<|>))
import           Control.Lens
import qualified Crypto.Hash as H
import qualified Data.HashMap.Strict as HMap
import           Data.Maybe
import           Data.String
-- import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Time.Clock
import qualified Filesystem.Path as P
import           Filesystem.Path.CurrentOS ()
import qualified Network.GitHub.Types.Gist as Gist

type GistFileId = (Gist.GistId, Gist.FileId)

-- | The sync status for a specific file
data SyncFile a = SyncFile
  { syncFilePath   :: P.FilePath
  , syncGistFileId :: GistFileId
  , syncFileHash   :: H.Digest a -- ^ Hash of the file when it was synced
  , syncFileTime   :: UTCTime    -- ^ The time of the sync
  } deriving (Show, Eq)

data LocalFileInfo a = LocalFileInfo
  { localFileHash         :: H.Digest a -- ^ Hash of the file as of a moment in time
  , localFileLastModified :: UTCTime    -- ^ Last modified time of the file
  } deriving (Show, Eq)

data SyncAction a = UpdateLocal  { localFilePath    :: P.FilePath
                                 , remoteGistFileId :: GistFileId
                                 , remoteFileURL    :: T.Text
                                 }
                  | CreateLocal  { localFilePath    :: P.FilePath
                                 , remoteGistFileId :: GistFileId
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
                  deriving (Show, Eq)

-- | A isomorphism between local file path and gist file id
newtype PathMapper = PathMapper { unPathMapper :: Iso' P.FilePath Gist.FileId }

pathMapper :: (P.FilePath -> Gist.FileId) -> (Gist.FileId -> P.FilePath) -> PathMapper
pathMapper t f = PathMapper $ iso t f

localToGistFile :: PathMapper -> P.FilePath -> Gist.FileId
localToGistFile (PathMapper is) = view is

gistToLocalFile :: PathMapper -> Gist.FileId -> P.FilePath
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
  -> [Gist.Gist]    -- ^ All gists from the remote end
  -> [Either e (SyncAction a)] -- ^ A list of actions with their resultant sync files
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
      [ ((p, localToGistFile pMapper p), (Just f, Nothing))
      | f@(eth,_) <- files
      , let p = either id syncFilePath eth
      ]
      ++
      [ ((gistToLocalFile pMapper fid, fid), (Nothing, Just (f, g)))
      | g <- gists
      , (fid, f) <- HMap.toList $ Gist.files g
      ]
    onPair
      :: ( P.FilePath, Gist.FileId )
      -> ( Maybe (Either P.FilePath (SyncFile a), LocalFileInfo a)
         , Maybe (Gist.File, Gist.Gist) )
      -> Maybe (Either e (SyncAction a))
    onPair (p,fid) (Nothing, Just (gfile, gist))
    -- never synced to local in the past
      = Just $ Right CreateLocal
      { localFilePath = p
      , remoteFileURL = Gist.fileRawUrl gfile
      , remoteGistFileId = (Gist.id gist, fid)
      }
    onPair (p,fid) (Just (Left _, local), Nothing)
    -- never synced to remote in the past
      = Just $ Right CreateRemote
      { localFilePath = p
      , localFileInfo = local
      , remoteFileId = fid
      }
    onPair (_,_) (Just (Right sync, _), Nothing)
      -- XXX: remote deletion..?
      = Just . Left . fromString $
        "Synced in the past, but no gist found; sync file = " ++ show sync
    onPair (p,fid) (Just (Right sync, local), Just (gfile, gist))
      | remoteChanged, not localChanged = Just $ Right UpdateLocal
        { localFilePath = p
        , remoteGistFileId = gistFileId
        , remoteFileURL = Gist.fileRawUrl gfile
        }
      | not remoteChanged, localChanged = Just $ Right UpdateRemote
        { localFilePath = p
        , localFileInfo = local
        , remoteGistFileId = gistFileId
        }
      | not remoteChanged, not localChanged = Nothing
      where localChanged = localFileHash local /= syncFileHash sync
            remoteChanged = Gist.updated_at gist > syncFileTime sync
            gistFileId = (Gist.id gist, fid)
    onPair (p,fid) (Just (_, local), Just (gfile, gist))
      -- both have changed, OR
      -- never synced, but there is a remote correspondence
      = Just $ Right SyncConflict
      { localFilePath = p
      , localFileInfo = local
      , remoteGistFileId = (Gist.id gist, fid)
      , remoteFileURL = Gist.fileRawUrl gfile
      , remoteUpdateTime = Gist.updated_at gist
      }
    onPair _ _ = Nothing
