getNothing : Task.Task Http.Error (())
getNothing =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "nothing"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.succeed ())
      (Http.send Http.defaultSettings request)
