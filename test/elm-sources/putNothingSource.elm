module PutNothingSource exposing (..)

import Http
import Url.Builder


putNothing : Http.Request ()
putNothing =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "PUT"
            , headers =
                []
            , url =
                Url.Builder.absolute
                    [ "nothing"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectStringResponse
                    (\ rsp  ->
                        if String.isEmpty rsp.body then
                            Ok ()
                        else
                            Err "Expected the response body to be empty"
                    )
            , timeout =
                Nothing
            , withCredentials =
                False
            }
