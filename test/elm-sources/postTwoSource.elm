module PostTwoSource exposing (..)

import Http
import Json.Decode exposing (..)
import Json.Encode
import Url.Builder


postTwo : String -> (Result Http.Error  ((Maybe Int))  -> msg) -> Cmd msg
postTwo body toMsg =
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
                Url.Builder.crossOrigin ""
                    [ "two"
                    ]
                    params
            , body =
                Http.jsonBody (Json.Encode.string body)
            , expect =
                Http.expectJson toMsg (Json.Decode.maybe (Json.Decode.int))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
