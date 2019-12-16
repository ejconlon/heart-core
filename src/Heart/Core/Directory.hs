module Heart.Core.Directory where

import Heart.Core.ListT
import Heart.Core.Prelude
import System.FilePath ((</>))
import UnliftIO.Directory (doesDirectoryExist, doesPathExist, listDirectory)

listDirectoryRecursively :: MonadIO m => FilePath -> ListT m FilePath
listDirectoryRecursively path = ListT $ do
  pathExists <- doesPathExist path
  if pathExists
    then do
      dirExists <- doesDirectoryExist path
      if dirExists
        then do
          subPaths <- listDirectory path
          let rest = asum (fmap (listDirectoryRecursively . (path </>)) subPaths)
          pure (Just (path, rest))
        else pure (Just (path, empty))
    else pure empty

searchDirectory :: MonadIO m => (FilePath -> m Bool) -> FilePath -> ListT m FilePath
searchDirectory pcate path = filterListT pcate (listDirectoryRecursively path)
