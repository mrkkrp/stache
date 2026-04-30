{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson
import Data.Text
import qualified Data.Text.Lazy.IO as TIO
import Text.Mustache

main :: IO ()
main = do
  template <- compileMustacheDir "main" "templates"
  TIO.putStr $
    renderMustache template $
      object
        [ "subnets" .= ["pen" :: Text, "candle", "egg"]
        ]
