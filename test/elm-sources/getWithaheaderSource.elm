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
                [ Maybe.map (Http.header "myStringHeader") header_myStringHeader
                , Maybe.map (Http.header "MyIntHeader" << toString) header_MyIntHeader
                , Maybe.map (Http.header "MyRequiredStringHeader") (Just header_MyRequiredStringHeader)
                , Maybe.map (Http.header "MyRequiredIntHeader" << toString) (Just header_MyRequiredIntHeader)
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
