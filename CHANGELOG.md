0.6.0.2
-------

* Ticket #53. Don't add a "query_" prefix to query params

0.6.0.1
-------

* Bug: Replace special symbols in query string param name

0.6.0.0
-------

* PR #49. Support Elm 0.19, migrate to elm-bridge, support http 2.0.0

0.5.0.0
-------
* Fix generation for APIs with response headers.
* Support Servant's `Optional` and `Required` modifiers for
  `Header`s and `QueryParam`s (`Header` arguments are now `Maybe`s
  by default). (phaazon) (#31)
* Filter out forbidden Cookie header. (xilnocas) (#37)

0.4.0.1
-------
* Remove hyphens from generated function names. (servant-foreign-0.10 no longer
  does this for us.)

0.4.0.0
-------
* Allow passing the base URL dynamically in Elm. (#20)
* Don't use `toString` on `Text` parameters. (domenkozar) (#23, #24)
* Fix query parameter generation. (domenkozar) (#25)

0.3.0.1
-------
* Prefix generated function arguments to ensure valid Elm identifiers (#21)
* Put integration tests behind a Cabal flag. (#22)

0.3.0.0
-------
* Update for Elm 0.18 and the new elm-lang/http library.
* Generated Elm functions now return an `Http.Request` value.

0.2.0.0
-------
* Use Text throughout the API.
* We no longer auto-generate Elm sources for the types, encoders and decoders
  used in your API - you must now use `elm-export`'s `toElmTypeSource` functions
  explicitly. See the tests and examples for usage.
* Allow setting options to pass to `elm-export`.
* Update to `servant-0.8` (purcell).
* Basic support for custom headers.
* Fix: `String` query params were being wrapped in double-quotes.
* Test: verify that the generated code can be compiled by Elm (soenkehahn)

0.1.0.2
-------
* Fix for API endpoints that return Unit (kantp)

0.1.0.1
-------
* Convenience re-exports from Elm and Data.Proxy.
* Add Haddoc documentation.

0.1
---
* Initial release.
