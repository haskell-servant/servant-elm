module GetNothingSource exposing (..)

import Http


getNothing : Http.Request NoContent
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
                            Ok NoContent
                        else
                            Err "Expected the response body to be empty"
                    )
            , timeout =
                Nothing
            , withCredentials =
                False
            }
