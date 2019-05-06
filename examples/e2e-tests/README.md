# End-to-end tests

Test that Elm can actually compile and run the code we generate.

Uses https://httpbin.org/ to generate sample responses.

### Usage

```
cd examples/e2e-tests
stack build
stack runghc generate.hs
elm install
elm reactor
```

Open http://localhost:8000/elm/Main.elm in your browser to check the results.
