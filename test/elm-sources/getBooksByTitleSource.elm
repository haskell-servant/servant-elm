module GetBooksByTitleSource exposing (..)

import Http
import String.Conversions as String
import Url


getBooksByTitle : String -> Http.Request (Http.Response (Book))
getBooksByTitle capture_title =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_title |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\res ->
                    Result.mapError Json.Decode.errorToString
                        (Result.map
                            (\body_ -> { url = res.url, status = res.status, headers = res.headers, body = body_ })
                            (decodeString decodeBook res.body)))
        , timeout =
            Nothing
        , withCredentials =
            False
        }
