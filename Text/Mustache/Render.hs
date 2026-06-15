{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Text.Mustache.Render
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for rendering Mustache templates. You don't usually need to
-- import the module, because "Text.Mustache" re-exports everything you may
-- need, import that module instead.
module Text.Mustache.Render
  ( renderMustache,
    renderMustacheW,
  )
where

import Control.Monad (forM_, unless, when)
import Data.Aeson hiding (Key)
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.Foldable (asum)
import Data.List (tails)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Encoding qualified as TL
import Data.Vector qualified as V
import Text.Mustache.Render.Internal
import Text.Mustache.Type

----------------------------------------------------------------------------
-- High-level interface

-- | Render a Mustache 'Template' using Aeson's 'Value' to get actual values
-- for interpolation.
renderMustache :: Template -> Value -> TL.Text
renderMustache t = snd . renderMustacheW t

-- | Like 'renderMustache', but also returns a collection of warnings.
--
-- @since 1.1.1
renderMustacheW :: Template -> Value -> ([MustacheWarning], TL.Text)
renderMustacheW template value =
  runR (renderTemplate (templateActual template)) value (templateCache template)

-- | Main template rendering function.
renderTemplate :: PName -> R ()
renderTemplate name = do
  mns <- lookupTemplate name
  case mns of
    Just ns -> mapM_ renderNode ns
    Nothing -> return ()

----------------------------------------------------------------------------
-- Node rendering

-- | Render a single 'Node'.
renderNode :: Node -> R ()
renderNode (TextBlock text) = txt IndentEveryLine text
renderNode (EscapedVar k) =
  lookupKey k >>= renderValue k >>= txt IndentOnlyFirstLine . escapeHtml
renderNode (UnescapedVar k) =
  lookupKey k >>= renderValue k >>= txt IndentOnlyFirstLine
renderNode (Section k ns) = do
  val <- lookupKey k
  unless (isBlank val) $
    addPrefix k $
      case val of
        Array xs ->
          forM_ (V.toList xs) $ \x ->
            addContext x (mapM_ renderNode ns)
        _ ->
          addContext val (mapM_ renderNode ns)
renderNode (InvertedSection k ns) = do
  val <- lookupKey k
  when (isBlank val) $
    mapM_ renderNode ns
renderNode (Partial pname mindent) = do
  mns <- lookupTemplate pname
  let adjustIndent = case mindent of
        Nothing -> id
        Just indent -> incrementIndentBy indent
  adjustIndent . resetPrefix $ case mns of
    Nothing -> txt IndentEveryLine ""
    Just ns -> mapM_ renderNode ns

----------------------------------------------------------------------------
-- Value lookup

-- | Lookup a 'Value' by its 'Key'.
lookupKey :: Key -> R Value
lookupKey (Key []) = NE.head <$> getContext
lookupKey k = do
  context <- getContext
  prefix <- getPrefix
  let f x = asum (simpleLookup False (x <> k) <$> context)
  case asum (fmap (f . Key) . reverse . tails $ unKey prefix) of
    Nothing ->
      Null <$ addWarning (MustacheVariableNotFound (prefix <> k))
    Just r ->
      return r

-- | Lookup a 'Value' by traversing another 'Value' using given 'Key' as "path".
simpleLookup ::
  -- | At least one part of the path matched, in this case we are
  -- “committed” to this lookup and cannot say “there is nothing, try
  -- other level”. This is necessary to pass the “Dotted Names—Context
  -- Precedence” test from the “interpolation.yml” spec.
  Bool ->
  -- | The key to lookup
  Key ->
  -- | Source value
  Value ->
  -- | Looked-up value
  Maybe Value
simpleLookup _ (Key []) obj = return obj
simpleLookup c (Key (k : ks)) (Object m) =
  case Aeson.KeyMap.lookup (Aeson.Key.fromText k) m of
    Nothing -> if c then Just Null else Nothing
    Just v -> simpleLookup True (Key ks) v
simpleLookup _ _ _ = Nothing

----------------------------------------------------------------------------
-- Helper functions

-- | Select invisible values.
isBlank :: Value -> Bool
isBlank Null = True
isBlank (Bool False) = True
isBlank (Object m) = Aeson.KeyMap.null m
isBlank (Array a) = V.null a
isBlank (String s) = T.null s
isBlank _ = False

-- | Render Aeson's 'Value' /without/ HTML escaping.
renderValue :: Key -> Value -> R Text
renderValue k v =
  case v of
    Null -> return ""
    String str -> return str
    Object _ -> do
      addWarning (MustacheDirectlyRenderedValue k)
      return $ TL.toStrict $ TL.decodeUtf8 $ encode v
    Array _ -> do
      addWarning (MustacheDirectlyRenderedValue k)
      return $ TL.toStrict $ TL.decodeUtf8 $ encode v
    _ -> return $ TL.toStrict $ TL.decodeUtf8 $ encode v

-- | Escape HTML represented as strict 'Text' in a single pass.
escapeHtml :: Text -> Text
escapeHtml = TL.toStrict . TLB.toLazyText . T.foldl' escapeChar mempty
  where
    escapeChar :: TLB.Builder -> Char -> TLB.Builder
    escapeChar acc c =
      acc <> case c of
        '&' -> TLB.fromText "&amp;"
        '<' -> TLB.fromText "&lt;"
        '>' -> TLB.fromText "&gt;"
        '"' -> TLB.fromText "&quot;"
        '\'' -> TLB.fromText "&#39;"
        _ -> TLB.singleton c
