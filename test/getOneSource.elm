getOne : Task.Task Http.Error (Int)
getOne =
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
          "/" ++ "one"
          ++ if List.isEmpty params then
               ""
             else
               "?" ++ String.join "," params
      , body =
          Http.empty
      }
  in
    Http.fromJson
      Json.Decode.int
      (Http.send Http.defaultSettings request)
