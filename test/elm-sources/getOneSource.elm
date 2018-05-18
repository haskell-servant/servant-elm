module GetOneSource exposing (..)

import Http
import Json.Decode exposing (..)


getOne : Http.Request Int
getOne =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "one"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson <| Json.Decode.int
        , timeout =
            Nothing
        , withCredentials =
            False
        }
