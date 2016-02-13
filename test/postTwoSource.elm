postTwo : String -> Task.Task Http.Error (Maybe Int)
postTwo body =
  let
    params =
      List.filter (not << String.isEmpty)
        [ ]
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "two"
          ++ if List.isEmpty params then
               ""
             else
               "?" ++ String.join "," params
      , body =
          Http.string (Json.Encode.encode 0 (Json.Encode.string body))
      }
  in
    Http.fromJson
      (Json.Decode.maybe Json.Decode.int)
      (Http.send Http.defaultSettings request)
