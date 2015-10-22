Requires servant 0.5 (not yet released). The `stack.yml` config expects the
servant sources to be in a sibling directory.

```
$ git clone https://github.com/haskell-servant/servant.git
$ git clone https://github.com/mattjbray/servant-elm.git
$ cd servant-elm
$ stack build
```

## Example

```
$ stack exec runhaskell examples/counter.hs
```

This will generate `examples/elm/Generated/Api.elm`. You can see an example Elm
app in `examples/elm/Main.elm`.