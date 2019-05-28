module GetWithAResponseHeaderSource exposing (..)

import Http
import Url.Builder
import Json.Decode exposing (..)


getWitharesponseheader : (Result Http.Error  (String)  -> msg) -> Cmd msg
getWitharesponseheader toMsg =
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
                Url.Builder.crossOrigin ""
                    [ "with-a-response-header"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg Json.Decode.string
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
