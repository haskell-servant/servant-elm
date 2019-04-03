module PostTwoSource exposing (..)

import Http
import Json.Decode exposing (..)
import Json.Encode
import Url.Builder


postTwo : String -> Http.Request (Maybe Int)
postTwo body =
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
                    [ "two"
                    ]
                    params
            , body =
                Http.jsonBody (Json.Encode.string body)
            , expect =
                Http.expectJson <| Json.Decode.maybe (Json.Decode.int)
            , timeout =
                Nothing
            , withCredentials =
                False
            }
