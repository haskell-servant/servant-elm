module GetWithAResponseHeaderSource exposing (..)

import Http
import Json.Decode exposing (..)


getWitharesponseheader : Http.Request String
getWitharesponseheader =
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
                    [ "with-a-response-header"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson <| Json.Decode.string
            , timeout =
                Nothing
            , withCredentials =
                False
            }
