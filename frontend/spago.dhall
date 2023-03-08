{ name = "skyscope-frontend"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "avar"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "formatters"
  , "js-timers"
  , "maybe"
  , "numbers"
  , "prelude"
  , "refs"
  , "signal"
  , "strings"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "web-dom"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
