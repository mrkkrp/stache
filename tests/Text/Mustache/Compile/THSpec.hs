{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Mustache.Compile.THSpec
  ( main,
    spec,
  )
where

import Data.Map qualified as M
import Test.Hspec
import Text.Mustache.Compile.TH qualified as TH
import Text.Mustache.Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mustache"
    $ it "compiles template using QuasiQuotes at compile time"
    $ [TH.mustache|From Quasi-Quote!
|]
      `shouldBe` qqTemplate
  describe "compileMustacheText"
    $ it "compiles template from text at compile time"
    $ $(TH.compileMustacheText "foo" "This is the ‘foo’.\n")
      `shouldBe` fooTemplate
  describe "compileMustacheFile"
    $ it "compiles template from file at compile time"
    $ $(TH.compileMustacheFile "templates/foo.mustache")
      `shouldBe` fooTemplate
  describe "compileMustacheDir"
    $ it "compiles templates from directory at compile time"
    $ $(TH.compileMustacheDir "foo" "templates/")
      `shouldBe` (fooTemplate <> barTemplate)

qqTemplate :: Template
qqTemplate =
  Template "quasi-quoted" $
    M.singleton "quasi-quoted" [TextBlock "From Quasi-Quote!\n"]

fooTemplate :: Template
fooTemplate =
  Template "foo" $
    M.singleton "foo" [TextBlock "This is the ‘foo’.\n"]

barTemplate :: Template
barTemplate =
  Template "bar" $
    M.singleton "bar" [TextBlock "And this is the ‘bar’.\n"]
