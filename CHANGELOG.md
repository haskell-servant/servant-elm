0.2.0.1
-------
* Use Text throughout the API.
* We no longer auto-generate Elm sources for the types, encoders and decoders
  used in your API - you must now use `elm-export`'s `toElmTypeSource` functions
  explicitly. See the tests and examples for usage.
* APIs with a response type of `()` are no longer supported - use Servant's
  `NoContent` type instead.
* Allow setting options to pass to `elm-export`.
* Update to `servant-0.8` (purcell).
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
