{ name = "skyscope-frontend"
, dependencies =
  [ "console"
  , "effect"
  , "maybe"
  , "prelude"
  , "unsafe-coerce"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
