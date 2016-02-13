To build this example:

```
cd examples/giphy
stack build
stack runghc generate.hs
elm package install
elm make elm/Main.elm
```

Then open index.html in your browser.
