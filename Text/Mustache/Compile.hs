-- |
-- Module      :  Text.Mustache.Compile
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Mustache 'Template' creation from file or a 'Text' value. You don't
-- usually need to import the module, because "Text.Mustache" re-exports
-- everything you may need, import that module instead.
module Text.Mustache.Compile
  ( compileMustacheDir,
    compileMustacheDir',
    getMustacheFilesInDir,
    getMustacheFilesInDir',
    isMustacheFile,
    compileMustacheFile,
    compileMustacheText,
  )
where

import Control.Exception
import Control.Monad (filterM, foldM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void
import System.Directory
import System.FilePath qualified as F
import Text.Megaparsec
import Text.Mustache.Parser
import Text.Mustache.Type

-- | Compile all templates in the specified directory and select one.
-- Template files should have the extension @mustache@, (e.g.
-- @foo.mustache@) to be recognized. This function /does not/ scan the
-- directory recursively.
--
-- Note that each template\/partial will get an identifier which consists of
-- the name of corresponding template file with extension @.mustache@
-- dropped. This is important for e.g. selecting active template after
-- loading (the first argument).
--
-- The action can throw 'MustacheParserException' and the same exceptions as
-- 'getDirectoryContents', and 'T.readFile'.
--
-- > compileMustacheDir = complieMustacheDir' isMustacheFile
compileMustacheDir ::
  (MonadIO m) =>
  -- | Which template to select after compiling
  PName ->
  -- | Directory with templates
  FilePath ->
  -- | The resulting template
  m Template
compileMustacheDir = compileMustacheDir' isMustacheFile

-- | The same as 'compileMustacheDir', but allows using a custom predicate
-- for template selection.
--
-- @since 1.2.0
compileMustacheDir' ::
  (MonadIO m) =>
  -- | Template selection predicate
  (FilePath -> Bool) ->
  -- | Which template to select after compiling
  PName ->
  -- | Directory with templates
  FilePath ->
  -- | The resulting template
  m Template
compileMustacheDir' predicate pname path =
  getMustacheFilesInDir' predicate path
    >>= fmap selectKey . foldM f (Template undefined M.empty)
  where
    selectKey t = t {templateActual = pname}
    f (Template _ old) fp = do
      Template _ new <- compileMustacheFile fp
      return (Template undefined (M.union new old))

-- | Return a list of templates found in given a directory. The returned
-- paths are absolute.
--
-- @since 0.2.2
getMustacheFilesInDir ::
  (MonadIO m) =>
  -- | Directory with templates
  FilePath ->
  m [FilePath]
getMustacheFilesInDir = getMustacheFilesInDir' isMustacheFile

-- | Return a list of templates that satisfy a predicate in a given
-- directory. The returned paths are absolute.
--
-- @since 1.2.0
getMustacheFilesInDir' ::
  (MonadIO m) =>
  -- | Mustache file selection predicate
  (FilePath -> Bool) ->
  -- | Directory with templates
  FilePath ->
  m [FilePath]
getMustacheFilesInDir' predicate path =
  liftIO $
    getDirectoryContents path
      >>= filterM f . fmap (F.combine path)
      >>= mapM makeAbsolute
  where
    f p = (&& predicate p) <$> doesFileExist p

-- | The default Mustache file predicate.
--
-- @since 1.2.0
isMustacheFile :: FilePath -> Bool
isMustacheFile path = F.takeExtension path == ".mustache"

-- | Compile a Mustache template and select it.
--
-- The action can throw 'MustacheParserException' and the same exceptions as
-- 'T.readFile'.
compileMustacheFile ::
  (MonadIO m) =>
  -- | Location of the file
  FilePath ->
  m Template
compileMustacheFile path = liftIO $ do
  input <- T.readFile path
  withException (compile input)
  where
    pname = pathToPName path
    compile = fmap (Template pname . M.singleton pname) . parseMustache path

-- | Compile a Mustache template from a 'Text' value. The cache will contain
-- only this template named according to given 'PName'.
compileMustacheText ::
  -- | How to name the template?
  PName ->
  -- | The template to compile
  Text ->
  -- | The result
  Either (ParseErrorBundle Text Void) Template
compileMustacheText pname txt =
  Template pname . M.singleton pname <$> parseMustache "" txt

----------------------------------------------------------------------------
-- Helpers

-- | Build a 'PName' from a given 'FilePath'.
pathToPName :: FilePath -> PName
pathToPName = PName . T.pack . F.takeBaseName

-- | Throw 'MustacheException' if the argument is 'Left' or return the
-- result inside 'Right'.
withException ::
  -- | Value to process
  Either (ParseErrorBundle Text Void) Template ->
  -- | The result
  IO Template
withException = either (throwIO . MustacheParserException) return
