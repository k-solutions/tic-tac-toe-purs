{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "tic-tic-toe"
, dependencies =
  [ "aff"
  , "arrays"
  , "assert"
  , "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "halogen-hooks"
  , "maybe"
  , "nonempty"
  , "prelude"
  , "spec"
  , "spec-discovery"
  , "test-unit"
  , "tuples"
  , "unfoldable"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
