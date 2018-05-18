module GetWithAResponseHeaderSource exposing (..)

import Http
import Json.Decode exposing (..)


getWitharesponseheader : Http.Request String
getWitharesponseheader =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "with-a-response-header"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson <| Json.Decode.string
        , timeout =
            Nothing
        , withCredentials =
            False
        }
