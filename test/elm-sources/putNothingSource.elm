module PutNothingSource exposing (..)

import String.Conversions as String
import Http


putNothing : (Result (Maybe Http.Metadata, Http.Error) (()) -> msg) -> Cmd msg
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
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata _ -> Err (Just metadata, Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            if String.isEmpty body_ then
                                Ok (())
                            else
                                Err (Just metadata, Http.BadBody <| "Expected the response body to be empty, but it was '" ++ body_ ++ "'.")
                            )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
