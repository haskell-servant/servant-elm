module GetBooksSource exposing (..)

import Http
import Json.Decode exposing (..)


getBooks : Bool -> (Maybe String) -> (Maybe Int) -> String -> (List (Maybe Bool)) -> Http.Request (List Book)
getBooks query_published query_sort query_year query_category query_filters =
    let
        params =
            List.filterMap identity
            (List.concat
                [ [ if query_published then
                    Just (Url.Builder.string "query_published" "")
                  else
                    Nothing ]
                , [ query_sort
                    |> Maybe.map (Url.Builder.string "sort") ]
                , [ query_year
                    |> Maybe.map (String.fromInt >> Url.Builder.string "year") ]
                , [ Just query_category
                    |> Maybe.map (Url.Builder.string "category") ]
                , query_filters
                    |> List.map (\val -> Just (Url.Builder.string "query_filters[]" (maybeBoolToIntStr val)))
                ])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.absolute
                    [ "books"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson <| Json.Decode.list (jsonDecBook)
            , timeout =
                Nothing
            , withCredentials =
                False
            }
