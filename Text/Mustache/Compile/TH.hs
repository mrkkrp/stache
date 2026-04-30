{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Text.Mustache.Compile.TH
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Template Haskell helpers to compile Mustache templates at compile time.
-- This module is not imported as part of "Text.Mustache", so you need to
-- import it yourself. Qualified import is recommended, but not necessary.
module Text.Mustache.Compile.TH
  ( compileMustacheDir,
    compileMustacheDir',
    compileMustacheFile,
    compileMustacheText,
    mustache,
  )
where

import Control.Exception
import Data.Text (Text)
import Data.Text qualified as T
import Language.Haskell.TH hiding (Dec)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (addDependentFile, lift)
import System.Directory
import Text.Mustache.Compile qualified as C
import Text.Mustache.Type

-- | Compile all templates in the specified directory and select one.
-- Template files should have the extension @mustache@, (e.g.
-- @foo.mustache@) to be recognized. This function /does not/ scan the
-- directory recursively.
--
-- This version compiles the templates at compile time.
--
-- > compileMustacheDir = compileMustacheDir' isMustacheFile
compileMustacheDir ::
  -- | Which template to select after compiling
  PName ->
  -- | Directory with templates
  FilePath ->
  -- | The resulting template
  Q Exp
compileMustacheDir = compileMustacheDir' C.isMustacheFile

-- | The same as 'compileMustacheDir', but allows using a custom predicate
-- for template selection.
--
-- This version compiles the templates at compile time.
--
-- @since 1.2.0
compileMustacheDir' ::
  -- | Template selection predicate
  (FilePath -> Bool) ->
  -- | Which template to select after compiling
  PName ->
  -- | Directory with templates
  FilePath ->
  -- | The resulting template
  Q Exp
compileMustacheDir' predicate pname path = do
  runIO (C.getMustacheFilesInDir' predicate path) >>= mapM_ addDependentFile
  (runIO . try) (C.compileMustacheDir' predicate pname path) >>= handleEither

-- | Compile a Mustache template and select it.
--
-- This version compiles the template at compile time.
compileMustacheFile ::
  -- | Location of the file
  FilePath ->
  Q Exp
compileMustacheFile path = do
  runIO (makeAbsolute path) >>= addDependentFile
  (runIO . try) (C.compileMustacheFile path) >>= handleEither

-- | Compile a Mustache template from 'Text' value. The cache will contain
-- only this template named according to given 'Key'.
--
-- This version compiles the template at compile time.
compileMustacheText ::
  -- | How to name the template?
  PName ->
  -- | The template to compile
  Text ->
  Q Exp
compileMustacheText pname text =
  (handleEither . either (Left . MustacheParserException) Right)
    (C.compileMustacheText pname text)

-- | Compile a Mustache using a QuasiQuoter. Usage:
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- > import Text.Mustache.Compile.TH (mustache)
-- >
-- > foo :: Template
-- > foo = [mustache|This is my inline {{ template }}.|]
--
-- Name of created partial is set to @"quasi-quoted"@. You can extend cache
-- of 'Template' created this way using @('Data.Semigroup.<>')@ and so work
-- with partials as usual.
--
-- @since 0.1.7
mustache :: QuasiQuoter
mustache =
  QuasiQuoter
    { quoteExp = compileMustacheText "quasi-quoted" . T.pack,
      quotePat = error "This usage is not supported.",
      quoteType = error "This usage is not supported.",
      quoteDec = error "This usage is not supported."
    }

-- | Given an 'Either' result return 'Right' and signal pretty-printed error
-- if we have a 'Left'.
handleEither :: Either MustacheException Template -> Q Exp
handleEither val =
  case val of
    Left err -> (fail . indentNicely . displayException) err
    Right template -> lift template
  where
    indentNicely x' =
      case lines x' of
        [] -> ""
        (x : xs) -> unlines (x : fmap (replicate 8 ' ' ++) xs)
