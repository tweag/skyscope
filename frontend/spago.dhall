{ name = "skyscope-frontend"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut-core"
  , "avar"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "http-methods"
  , "maybe"
  , "prelude"
  , "record"
  , "refs"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
