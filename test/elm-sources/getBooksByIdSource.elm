module GetBooksByIdSource exposing (..)

import String.Conversions as String
import Http
import Url


getBooksById : Int -> Http.Request (Http.Response (Book))
getBooksById capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_id |> String.fromInt |> Url.percentEncode
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
