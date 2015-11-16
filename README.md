Requires servant 0.5 (not yet released).

```
$ git clone https://github.com/mattjbray/servant-elm.git
$ cd servant-elm
$ stack build
```

## Example

See `examples/README.md`, or take a look at https://github.com/mattjbray/servant-elm-example-app.

## TODO

Servant API coverage:

* MatrixFlag / MatrixParam / MatrixParams
* Header (request)
* Headers (response)
* Delete / Patch / Put / Raw
* ReqBody
* Vault / RemoteHost / IsSecure

Other:

* Generate Elm Json encoders for ReqBody?
* ToText in Elm for captures/params?
* Option to not use elm-export: generate functions that take a decoder and
  String arguments.
