let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221207/packages.dhall
        sha256:2ae34b92177adee8938984859e554ef43b0d68736717856edb90effda076aa2a

in  upstream
  with deku =
    { dependencies =
      [ "aff"
      , "arrays"
      , "bolson"
      , "catenable-lists"
      , "control"
      , "effect"
      , "either"
      , "fast-vect"
      , "filterable"
      , "foldable-traversable"
      , "foreign-object"
      , "hyrule"
      , "maybe"
      , "newtype"
      , "ordered-collections"
      , "prelude"
      , "profunctor"
      , "quickcheck"
      , "record"
      , "safe-coerce"
      , "st"
      , "strings"
      , "transformers"
      , "tuples"
      , "unsafe-coerce"
      , "web-dom"
      , "web-events"
      , "web-html"
      , "web-uievents"
      ]
    , repo = "https://github.com/mikesol/purescript-deku.git"
    , version = "v0.9.9"
    }
  with csv = {
    dependencies =
      [ "arrays"
      , "control"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "lists"
      , "maybe"
      , "ordered-collections"
      , "parsing"
      , "prelude"
      , "strings"
      , "test-unit"
      ]
    , repo = "https://github.com/jsparkes/purescript-csv.git"
    , version = "master"
    }

