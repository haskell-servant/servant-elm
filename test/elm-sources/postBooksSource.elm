module PostBooksSource exposing (..)

import String.Conversions as String
import Http
import SimulatedEffect.Http
import ProgramTest


postBooks : (Result (Maybe (Http.Metadata, String), Http.Error) (NoContent) -> msg) -> Book -> Cmd msg
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
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            if String.isEmpty body_ then
                                Ok (NoContent)
                            else
                                Err (Just (metadata, body_), Http.BadBody <| "Expected the response body to be empty, but it was '" ++ body_ ++ "'.")
                            
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postBooksSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (NoContent) -> msg) -> Book -> ProgramTest.SimulatedEffect msg
postBooksSimulated toMsg body =
    SimulatedEffect.Http.request
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
            SimulatedEffect.Http.jsonBody (encodeBook body)
        , expect =
            SimulatedEffect.Http.expectStringResponse toMsg
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            if String.isEmpty body_ then
                                Ok (NoContent)
                            else
                                Err (Just (metadata, body_), Http.BadBody <| "Expected the response body to be empty, but it was '" ++ body_ ++ "'.")
                            
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
