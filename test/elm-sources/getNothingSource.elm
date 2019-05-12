module GetNothingSource exposing (..)

import Http
import Url.Builder


getNothing : Http.Request ()
getNothing =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
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
