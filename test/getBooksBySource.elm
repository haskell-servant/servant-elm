getBooksBy : Int -> Task.Task Http.Error (Book)
getBooksBy id =
  let
    params =
      List.filter (not << String.isEmpty)
        [ ]
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "books"
          ++ "/" ++ (id |> toString |> Http.uriEncode)
          ++ if List.isEmpty params then
               ""
             else
               "?" ++ String.join "," params
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeBook
      (Http.send Http.defaultSettings request)
