{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.Mustache.Render.Internal
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal machinery of the rendering monad.
--
-- @since 2.4.0
module Text.Mustache.Render.Internal
  ( R,
    runR,
    IndentationPolicy (..),
    txt,
    incrementIndentBy,
    addContext,
    addPrefix,
    resetPrefix,
    addWarning,
    getContext,
    getPrefix,
    lookupTemplate,
  )
where

import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.State.Strict (State, execState, modify')
import Data.Aeson (Value)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Text.Megaparsec.Pos (Pos, mkPos, pos1, unPos)
import Text.Mustache.Type (Key, MustacheWarning, Node, PName)

-- | The 'R' monad supports printing with some helpful functionality to deal
-- with indentation.
newtype R a = R (ReaderT RC (State SC) a)
  deriving (Functor, Applicative, Monad)

-- | The reader context of 'R'.
data RC = RC
  { -- | Indentation level, as the column index we need to start from after
    -- a newline
    rcIndent :: !Pos,
    -- | The context stack
    rcContext :: NonEmpty Value,
    -- | The prefix accumulated by entering sections
    rcPrefix :: Key,
    -- | The template cache
    rcTemplateCache :: Map PName [Node]
  }

-- | The state context of 'R'.
data SC = SC
  { -- | Index of the next column to render
    scColumn :: !Pos,
    -- | Are we at the beginning of a newline?
    scBeginningOfLine :: !Bool,
    -- | Mustache warnings
    scWarnings :: [MustacheWarning] -> [MustacheWarning],
    -- | The output we have rendered so far (using Builder for efficiency)
    scBuilder :: TLB.Builder
  }

-- | The indentation policy to use when rendering text.
data IndentationPolicy
  = -- | Indent every line
    IndentEveryLine
  | -- | Only indent the very first line, provided we are at the beginning
    -- of the line
    IndentOnlyFirstLine

-- | Run the 'R' monad.
runR ::
  -- | Monad to run
  R () ->
  -- | The Context to use
  Value ->
  -- | The template cache
  Map PName [Node] ->
  ([MustacheWarning], TL.Text)
runR (R m) v cache = (scWarnings s [], TLB.toLazyText (scBuilder s))
  where
    rc =
      RC
        { rcIndent = pos1,
          rcContext = v :| [],
          rcPrefix = mempty,
          rcTemplateCache = cache
        }
    sc =
      SC
        { scColumn = pos1,
          scBeginningOfLine = True,
          scWarnings = id,
          scBuilder = mempty
        }
    s = execState (runReaderT m rc) sc

-- | Output text, handling indentation appropriately. When we're at the
-- beginning of a line and the indentation level is non-zero, we first
-- output the appropriate amount of spaces.
txt ::
  -- | Indentation policy to use
  IndentationPolicy ->
  -- | The text to output
  Text ->
  R ()
txt _ "" = R $ do
  indent <- asks rcIndent
  modify' $ \sc ->
    if scBeginningOfLine sc && indent > pos1
      then
        let indentText = T.replicate (unPos indent - 1) " "
         in sc
              { scColumn = mkPos (unPos indent),
                scBeginningOfLine = False,
                scBuilder = scBuilder sc <> TLB.fromText indentText
              }
      else sc
txt policy t = R $ do
  indent <- asks rcIndent
  modify' $ \sc ->
    let processLine isFirstLine line sc' =
          let shouldIndent = case policy of
                IndentEveryLine ->
                  scBeginningOfLine sc'
                    && not (T.null line)
                    && indent > pos1
                IndentOnlyFirstLine ->
                  isFirstLine
                    && scBeginningOfLine sc'
                    && not (T.null line)
                    && indent > pos1
              indentText =
                if shouldIndent
                  then T.replicate (unPos indent - 1) " "
                  else T.empty
              indentedLine = indentText <> line
              newColumn =
                if T.null indentedLine
                  then scColumn sc'
                  else mkPos (unPos (scColumn sc') + T.length indentedLine)
           in sc'
                { scColumn = newColumn,
                  scBeginningOfLine = T.null indentedLine && scBeginningOfLine sc',
                  scBuilder = scBuilder sc' <> TLB.fromText indentedLine
                }
        go _ [] sc' = sc'
        go isFirst [line] sc' = processLine isFirst line sc'
        go isFirst (line : rest) sc' =
          let sc'' = processLine isFirst line sc'
              sc''' =
                sc''
                  { scColumn = pos1,
                    scBeginningOfLine = True,
                    scBuilder = scBuilder sc'' <> TLB.singleton '\n'
                  }
           in go False rest sc'''
        lines' = T.lines t
        endsWithNewline = not (T.null t) && T.last t == '\n'
        processedState = go True lines' sc
     in if endsWithNewline
          then
            processedState
              { scColumn = pos1,
                scBeginningOfLine = True,
                scBuilder = scBuilder processedState <> TLB.singleton '\n'
              }
          else processedState

-- | Increment the indentation level by the given amount.
incrementIndentBy :: Pos -> R a -> R a
incrementIndentBy n (R m) =
  R $
    local (\rc -> rc {rcIndent = mkPos (unPos (rcIndent rc) + unPos n - 1)}) m

-- | Add a value to the context stack.
addContext :: Value -> R a -> R a
addContext v (R m) =
  R $
    local (\rc -> rc {rcContext = NE.cons v (rcContext rc)}) m

-- | Add to the key prefix.
addPrefix :: Key -> R a -> R a
addPrefix key (R m) =
  R $
    local (\rc -> rc {rcPrefix = rcPrefix rc <> key}) m

-- | Reset prefix.
resetPrefix :: R a -> R a
resetPrefix (R m) =
  R $
    local (\rc -> rc {rcPrefix = mempty}) m

-- | Add a warning to the warnings list.
addWarning :: MustacheWarning -> R ()
addWarning w = R $
  modify' $
    \sc -> sc {scWarnings = scWarnings sc . (w :)}

-- | Get the current context stack.
getContext :: R (NonEmpty Value)
getContext = R $ asks rcContext

-- | Get the current key prefix.
getPrefix :: R Key
getPrefix = R $ asks rcPrefix

-- | Lookup the template by its name.
lookupTemplate :: PName -> R (Maybe [Node])
lookupTemplate name = R $ asks $ \rc ->
  M.lookup name (rcTemplateCache rc)
