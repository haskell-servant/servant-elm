getBooks : Bool -> Maybe (String) -> List (Maybe Bool) -> Task.Task Http.Error (List (Book))
getBooks published sort filters =
  let
    params =
      List.filter (not << String.isEmpty)
        [ if published then
            "published="
          else
            ""
        , sort
            |> Maybe.map (toString >> Http.uriEncode >> (++) "sort=")
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
          "/" ++ "books"
          ++ if List.isEmpty params then
               ""
             else
               "?" ++ String.join "&" params
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeBook)
      (Http.send Http.defaultSettings request)
