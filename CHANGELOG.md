## Unreleased

* Fix indentation inheritance in partials. [Issue
  72](https://github.com/mrkkrp/stache/issues/72).

* Changing license back to BSD 3 clause.

## Stache 2.3.4

* Support GHC 9.6 thanks to @ysangkok
* Fix infinite loop caused by unclosed sections thanks to @ernius

## Stache 2.3.3

* Allow `vector-0.13`
* Changing license from BSD-3 to MIT

## Stache 2.3.2

* Builds with `mtl-2.3`.

## Stache 2.3.1

* Uses `aeson-2.x.x.x`.

## Stache 2.3.0

* Allowed parsing a wider range of keys in the templates thus making the
  library's behavior closer to other existing implementations.

## Stache 2.2.1

* Works with GHC 9.0.1, dropped support for GHC 8.6.x and older.

## Stache 2.2.0

* Added the executable program.

## Stache 2.1.1

* Fixed the bug related to incorrect indentation with nested partials.
  [Issue 44](https://github.com/stackbuilders/stache/issues/44).

* Dropped support for GHC 8.2 and older.

## Stache 2.1.0

* Dropped support for GHC 8.0 and older.

* Documentation improvements.

* Added Template Haskell `Lift` instance for the `Template` type and its
  sub-components.

## Stache 2.0.1

* Now interpolation with escaping also escapes single quotes as `&#39`.

## Stache 2.0.0

* Uses Megaparsec 7. In particular, the parser now returns
  `ParseErrorBundle` on failure and the bundle is also inside
  `MustacheException`.

## Stache 1.2.1

* Fixed a bug in `compileMustacheDir'` from `Text.Mustache.Compile.TH`: it
  did not use the supplied predicate and selected only `.mustache` files
  like `compileMustacheDir`.

## Stache 1.2.0

* Added `compileMustacheDir'` and `getMusthacheFilesInDir'` that allow for a
  custom template predicate. Also exposed `isMustacheFile` as the default
  predicate used by functions like `compileMustacheDir`.

* Abandon attempts to support GHC 7.8.

## Stache 1.1.2

* Fixed compilation of the test suite with Cabal 2.0/GHC 8.2.1.

## Stache 1.1.1

* Exported `mustacheRenderW`, `MustacheWarning`, and
  `displayMustacheWarning` from `Text.Mustache`.

## Stache 1.1.0

* Added `mustacheRenderW` that allows to render a `Template` collecting
  warnings in the process. Also added the `MusthacheWarning` type
  representing the warnings themselves and `displayMustacheWarning` to
  pretty-print the warnings.

## Stache 1.0.0

* Improved metadata and documentation.

* Breaking change: the `renderMustache` function does not throw exceptions
  when referenced key is not provided as per the spec. This is the behaviour
  we had before 0.2.0, and it played better with the rest of Mustache.
  Correspondingly, `MustacheRenderException` was removed.

* Stache now uses Megaparsec 6 for parsing.

* `MustacheException` now includes original input as `Text`.

* `compileMustacheText` and `parseMustache` now accept strict `Text` instead
  of lazy `Text`.

* Minor improvement in rendering speed.

* Stop depending on `exceptions` and drop `MonadThrow` constraints.

## Stache 0.2.2

* Add the `getMustacheFilesInDir` function.

* Make TH helpers `compileMustacheDir` and `compileMustacheFile` detect
  changes in the templates and force recompilation.

## Stache 0.2.1

* Made TH parse errors nicer.

## Stache 0.2.0

* Breaking change: the `renderMustache` function will throw an exception
  when referenced key was not provided. This is a better behavior than
  silent interpolation of an empty string, because missing values are almost
  always a mistake and it's easy to provide empty strings explicitly anyway.

* Allowed `directory-1.3.0.0`.

## Stache 0.1.8

* Rename `specs` directory to `specification` as the previous name somehow
  caused conflicts when deploying an application on Heroku with Stache as a
  dependency.

## Stache 0.1.7

* Added `mustache` quasi-quoter.

## Stache 0.1.6

* Fixed a bug in the lookup algorithm for dot-separated keys.

## Stache 0.1.5

* When section's key is a number or non-empty string, it's accessible as `.`
  in the section body.

* Allow Aeson 1.0.

## Stache 0.1.4

* Numbers are now treated as “true” values.

* Added `Semigroup` instance for `Key`.

* Now change of delimiters affects special unescaped variable syntax as
  well, for example: `{{=<< >>=}}<<{var}>>` will be parsed as unescaped
  variable `var`.

* `compileMustacheFile` now shows full path to malformed template when the
  template cannot be parsed.

* Defined `displayException` method of `Exception` type class for
  `MustacheException` using `parseErrorPretty` from Megaparsec.

## Stache 0.1.3

* Cosmetic improvements.

* Minor improvement in performance of parser.

## Stache 0.1.2

* Fixed compilation of benchmarks with Megaparsec 5.0.1 and later.

## Stache 0.1.1

* Added benchmarks.

## Stache 0.1.0

* Initial release.
