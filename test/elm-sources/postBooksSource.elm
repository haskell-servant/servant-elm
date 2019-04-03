module PostBooksSource exposing (..)

import Http


postBooks : Book -> Http.Request NoContent
postBooks body =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.absolute
                    [ "books"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncBook body)
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
