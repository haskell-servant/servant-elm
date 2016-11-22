module GetBooksSource exposing (..)

import Http
import Json.Decode exposing (..)


getBooks : Bool -> Maybe (String) -> Maybe (Int) -> List (Maybe (Bool)) -> Http.Request (List (Book))
getBooks published sort year filters =
    let
        params =
            List.filter (not << String.isEmpty)
                [ if published then
                    "published="
                  else
                    ""
                , sort
                    |> Maybe.map (Http.encodeUri >> (++) "sort=")
                    |> Maybe.withDefault ""
                , year
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "year=")
                    |> Maybe.withDefault ""
                , filters
                    |> List.map (\val -> "filters[]=" ++ (val |> toString |> Http.encodeUri))
                    |> String.join "&"
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                [ Http.header "Content-Type" "application/json"
                ]
            , url =
                String.join "/"
                    [ ""
                    , "books"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson (list decodeBook)
            , timeout =
                Nothing
            , withCredentials =
                False
            }
