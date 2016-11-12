getOne : Task.Task Http.Error (Int)
getOne =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          String.join "/"
            [ ""
            , "one"
            ]
      , body =
          Http.empty
      }
  in
    Http.fromJson
      int
      (Http.send Http.defaultSettings request)
