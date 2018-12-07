module PutNothingSource exposing (..)

import String.Conversions as String
import Http


putNothing : (Result Http.Error (()) -> msg) -> Cmd msg
putNothing toMsg =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "nothing"
                ]
        , body =
            Http.emptyBody
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
                                Ok (())
                            else
                                Err (Http.BadBody "Expected the response body to be empty")
                            )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
