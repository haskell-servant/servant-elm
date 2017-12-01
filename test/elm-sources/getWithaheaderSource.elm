module GetWithAHeaderSource exposing (..)

import Http
import Json.Decode exposing (..)


getWithaheader : Maybe (String) -> Maybe (Int) -> Http.Request (String)
getWithaheader header_myStringHeader header_MyIntHeader =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity [ Maybe.map (Http.header "myStringHeader" << identity) header_myStringHeader
            , Maybe.map (Http.header "MyIntHeader" << toString) header_MyIntHeader
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
