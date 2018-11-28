module GetWithAHeaderSource exposing (..)

import Http
import String.Conversions as String
import Json.Decode exposing (..)


getWithaheader : String -> Int -> Http.Request (Http.Response (String))
getWithaheader header_myStringHeader header_MyIntHeader =
    Http.request
        { method =
            "GET"
        , headers =
            [ Http.header "myStringHeader" (header_myStringHeader)
            , Http.header "MyIntHeader" (String.fromInt header_MyIntHeader)
            ]
        , url =
            String.join "/"
                [ ""
                , "with-a-header"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\res ->
                    Result.mapError Json.Decode.errorToString
                        (Result.map
                            (\body_ -> { url = res.url, status = res.status, headers = res.headers, body = body_ })
                            (decodeString string res.body)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }
