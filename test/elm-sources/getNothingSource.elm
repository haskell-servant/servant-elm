module GetNothingSource exposing (..)

import Http


getNothing : Http.Request (NoContent)
getNothing =
    Http.request
        { method =
            "GET"
        , headers =
            [ Http.header "Content-Type" "application/json"
            ]
        , url =
            String.join "/"
                [ ""
                , "nothing"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }
