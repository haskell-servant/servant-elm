getBooks : Bool -> Maybe (String) -> Maybe (Int) -> List (Maybe (Bool)) -> Task.Task Http.Error (List (Book))
getBooks published sort year filters =
  let
    params =
      List.filter (not << String.isEmpty)
        [ if published then
            "published="
          else
            ""
        , sort
            |> Maybe.map (Http.uriEncode >> (++) "sort=")
            |> Maybe.withDefault ""
        , year
            |> Maybe.map (toString >> Http.uriEncode >> (++) "year=")
            |> Maybe.withDefault ""
        , filters
            |> List.map (\val -> "filters[]=" ++ (val |> toString |> Http.uriEncode))
            |> String.join "&"
        ]
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
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
          Http.empty
      }
  in
    Http.fromJson
      (list decodeBook)
      (Http.send Http.defaultSettings request)
