postTwo : String -> Task.Task Http.Error (Maybe (Int))
postTwo body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          String.join "/"
            [ ""
            , "two"
            ]
      , body =
          Http.string (Json.Encode.encode 0 (Json.Encode.string body))
      }
  in
    Http.fromJson
      (maybe int)
      (Http.send Http.defaultSettings request)
