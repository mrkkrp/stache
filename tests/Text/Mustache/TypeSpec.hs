{-# LANGUAGE OverloadedStrings #-}

module Text.Mustache.TypeSpec
  ( main,
    spec,
  )
where

import Data.Map qualified as M
import Test.Hspec
import Text.Mustache.Type

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Template instances" $
    context "the Semigroup instance" $ do
      it "the resulting template inherits focus of the left one" $
        templateActual (templateA <> templateB)
          `shouldBe` templateActual templateA
      it "the resulting template merges caches with left bias" $
        templateCache (templateA <> templateB)
          `shouldBe` M.fromList
            [ ("c", [TextBlock "foo"]),
              ("d", [TextBlock "bar"]),
              ("e", [TextBlock "baz"])
            ]
  describe "showKey" $ do
    context "when the key has no elements in it"
      $ it "is rendered correctly"
      $ showKey (Key []) `shouldBe` "<implicit>"
    context "when the key has some elements" $
      it "is rendered correctly" $ do
        showKey (Key ["boo"]) `shouldBe` "boo"
        showKey (Key ["foo", "bar"]) `shouldBe` "foo.bar"
        showKey (Key ["baz", "baz", "quux"]) `shouldBe` "baz.baz.quux"
  describe "displayMustacheWarning" $ do
    it "renders “not found” warning correctly" $
      displayMustacheWarning (MustacheVariableNotFound (Key ["foo"]))
        `shouldBe` "Referenced value was not provided, key: foo"
    it "renders “directly rendered value” warning correctly" $
      displayMustacheWarning (MustacheDirectlyRenderedValue (Key ["foo"]))
        `shouldBe` "Complex value rendered as such, key: foo"

templateA :: Template
templateA =
  Template "a" $
    M.fromList
      [ ("c", [TextBlock "foo"]),
        ("d", [TextBlock "bar"])
      ]

templateB :: Template
templateB =
  Template "b" $
    M.fromList
      [ ("c", [TextBlock "bar"]),
        ("e", [TextBlock "baz"])
      ]
