module GetBooksByTitleSource exposing (..)

import Http
import String.Conversions as String
import Url


getBooksByTitle : (Result (Maybe (Http.Metadata, String), Http.Error) (Book) -> msg) -> String -> Cmd msg
getBooksByTitle toMsg capture_title =
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
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString decodeBook body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_))))
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
