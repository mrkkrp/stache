{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Text.Mustache.Type
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types used in the package. You don't usually need to import the module,
-- because "Text.Mustache" re-exports everything you may need, import that
-- module instead.
module Text.Mustache.Type
  ( Template (..),
    Node (..),
    Key (..),
    showKey,
    PName (..),
    MustacheException (..),
    MustacheWarning (..),
    displayMustacheWarning,
  )
where

import Control.DeepSeq
import Control.Exception (Exception (..))
import Data.Data (Data)
import Data.Map (Map)
import Data.Map qualified as M
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (cast)
import Data.Void
import GHC.Generics
import Language.Haskell.TH.Syntax qualified as TH
import Text.Megaparsec

-- | Mustache template as the name of the “top-level” template and a
-- collection of all available templates (partials).
--
-- 'Template' is a 'Semigroup'. This means that you can combine 'Template's
-- (and their caches) using the @('<>')@ operator, the resulting 'Template'
-- will have the same currently selected template as the left one. Union of
-- caches is also left-biased.
data Template = Template
  { -- | The name of the currently “selected” template.
    templateActual :: PName,
    -- | A collection of all templates that are available for interpolation
    -- (as partials). The top-level one is also contained here and the
    -- “focus” can be switched easily by modifying 'templateActual'.
    templateCache :: Map PName [Node]
  }
  deriving (Eq, Ord, Show, Data, Generic)

instance Semigroup Template where
  (Template pname x) <> (Template _ y) = Template pname (M.union x y)

-- | @since 2.1.0
instance TH.Lift Template where
  lift = liftData
  liftTyped = TH.Code . TH.unsafeTExpCoerce . TH.lift

-- | A structural element of a template.
data Node
  = -- | Plain text contained between tags
    TextBlock Text
  | -- | HTML-escaped variable
    EscapedVar Key
  | -- | Unescaped variable
    UnescapedVar Key
  | -- | Mustache section
    Section Key [Node]
  | -- | Inverted section
    InvertedSection Key [Node]
  | -- | Partial with indentation level ('Nothing' means it was inlined)
    Partial PName (Maybe Pos)
  deriving (Eq, Ord, Show, Data, Generic)

-- | @since 2.1.0
instance TH.Lift Node where
  lift = liftData
  liftTyped = TH.Code . TH.unsafeTExpCoerce . TH.lift

-- | Identifier for values to interpolate.
--
-- The representation is the following:
--
--     * @[]@—empty list means implicit iterators;
--     * @[text]@—single key is a normal identifier;
--     * @[text1, text2]@—multiple keys represent dotted names.
newtype Key = Key {unKey :: [Text]}
  deriving (Eq, Ord, Show, Semigroup, Monoid, Data, Generic)

instance NFData Key

-- | @since 2.1.0
instance TH.Lift Key where
  lift = liftData
  liftTyped = TH.Code . TH.unsafeTExpCoerce . TH.lift

-- | Pretty-print a key. This is helpful, for example, if you want to
-- display an error message.
--
-- @since 0.2.0
showKey :: Key -> Text
showKey (Key []) = "<implicit>"
showKey (Key xs) = T.intercalate "." xs

-- | Identifier for partials. Note that with the @OverloadedStrings@
-- extension you can use just string literals to create values of this type.
newtype PName = PName {unPName :: Text}
  deriving (Eq, Ord, Show, Data, Generic)

instance IsString PName where
  fromString = PName . T.pack

instance NFData PName

-- | @since 2.1.0
instance TH.Lift PName where
  lift = liftData
  liftTyped = TH.Code . TH.unsafeTExpCoerce . TH.lift

-- | Exception that is thrown when parsing of a template fails or referenced
-- values are not provided.
newtype MustacheException
  = -- | Template parser has failed. This contains the parse error.
    --
    -- /Before version 0.2.0 it was called 'MustacheException'./
    --
    -- /The 'Text' field was added in version 1.0.0./
    MustacheParserException (ParseErrorBundle Text Void)
  deriving (Eq, Show, Generic)

instance Exception MustacheException where
  displayException (MustacheParserException b) = errorBundlePretty b

-- | Warning that may be generated during rendering of a 'Template'.
--
-- @since 1.1.1
data MustacheWarning
  = -- | The template contained a variable for which there was no data in
    -- the current context.
    MustacheVariableNotFound Key
  | -- | A complex value such as an 'Object' or 'Array' was directly
    -- rendered into the template.
    MustacheDirectlyRenderedValue Key
  deriving (Eq, Show, Generic)

-- | Pretty-print a 'MustacheWarning'.
--
-- @since 1.1.1
displayMustacheWarning :: MustacheWarning -> String
displayMustacheWarning (MustacheVariableNotFound key) =
  "Referenced value was not provided, key: " ++ T.unpack (showKey key)
displayMustacheWarning (MustacheDirectlyRenderedValue key) =
  "Complex value rendered as such, key: " ++ T.unpack (showKey key)

----------------------------------------------------------------------------
-- TH lifting helpers

liftData :: (Data a, TH.Quote m) => a -> m TH.Exp
liftData = TH.dataToExpQ (fmap liftText . cast)

liftText :: (TH.Quote m) => Text -> m TH.Exp
liftText t = TH.AppE (TH.VarE 'T.pack) <$> TH.lift (T.unpack t)
