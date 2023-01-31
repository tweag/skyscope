{ name = "skyscope-frontend"
, dependencies =
  [ "console"
  , "effect"
  , "maybe"
  , "prelude"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
