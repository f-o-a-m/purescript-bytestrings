{ name = "bytestrings"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "gen"
  , "integers"
  , "leibniz"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "nonempty"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "quickcheck-laws"
  , "quotient"
  , "strings"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
