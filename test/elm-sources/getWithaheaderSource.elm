module GetWithAHeaderSource exposing (..)

import Http
import Url.Builder
import Json.Decode exposing (..)


getWithaheader : (Maybe String) -> (Maybe Int) -> String -> Int -> (Result Http.Error  (String)  -> msg) -> Cmd msg
getWithaheader header_myStringHeader header_MyIntHeader header_MyRequiredStringHeader header_MyRequiredIntHeader toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                List.filterMap identity
                    [ Maybe.map (Http.header "myStringHeader") header_myStringHeader
                    , Maybe.map (Http.header "MyIntHeader" << String.fromInt) header_MyIntHeader
                    , Maybe.map (Http.header "MyRequiredStringHeader") (Just header_MyRequiredStringHeader)
                    , Maybe.map (Http.header "MyRequiredIntHeader" << String.fromInt) (Just header_MyRequiredIntHeader)
                    ]
            , url =
                Url.Builder.crossOrigin ""
                    [ "with-a-header"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg Json.Decode.string
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
