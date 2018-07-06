module GetBooksSource exposing (..)

import Http
import Json.Decode exposing (..)


getBooks : Bool -> Maybe (String) -> Maybe (Int) -> String -> List (Maybe (Bool)) -> Http.Request (List (Book))
getBooks query_published query_sort query_year query_category query_filters =
    let
        params =
            List.filter (not << String.isEmpty)
                [ if query_published then
                    "published="
                  else
                    ""
                , query_sort
                    |> Maybe.map (Http.encodeUri >> (++) "sort=")
                    |> Maybe.withDefault ""
                , query_year
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "year=")
                    |> Maybe.withDefault ""
                , Just query_category
                    |> Maybe.map (Http.encodeUri >> (++) "category=")
                    |> Maybe.withDefault ""
                , query_filters
                    |> List.map (\val -> "filters[]=" ++ (val |> toString |> Http.encodeUri))
                    |> String.join "&"
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
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
