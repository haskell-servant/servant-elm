module PostBooksSource exposing (..)

import String.Conversions as String
import Http


postBooks : (Result Http.Error (NoContent) -> msg) -> Book -> Cmd msg
postBooks toMsg body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                ]
        , body =
            Http.jsonBody (encodeBook body)
        , expect =
            Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Http.BadUrl url)
                        Http.Timeout_ -> Err Http.Timeout
                        Http.NetworkError_ -> Err Http.NetworkError
                        Http.BadStatus_ metadata _ -> Err (Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            if String.isEmpty body_ then
                                Ok (NoContent)
                            else
                                Err (Http.BadBody "Expected the response body to be empty")
                            )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
