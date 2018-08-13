module GetWithAHeaderSource exposing (..)

import Http
import Json.Decode exposing (..)


getWithaheader : Maybe (String) -> Maybe (Int) -> String -> Int -> Http.Request (String)
getWithaheader header_myStringHeader header_MyIntHeader header_MyRequiredStringHeader header_MyRequiredIntHeader =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (identity >> Http.header "myStringHeader") header_myStringHeader
                , Maybe.map (String.fromInt >> Http.header "MyIntHeader") header_MyIntHeader
                , Maybe.map (Http.header "MyRequiredStringHeader") (Just header_MyRequiredStringHeader)
                , Maybe.map (String.fromInt >> Http.header "MyRequiredIntHeader") (Just header_MyRequiredIntHeader)
                ]
        , url =
            String.join "/"
                [ ""
                , "with-a-header"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }
