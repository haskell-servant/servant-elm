postBooks : Book -> Task.Task Http.Error (NoContent)
postBooks body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          String.join "/"
            [ ""
            , "books"
            ]
      , body =
          Http.string (Json.Encode.encode 0 (encodeBook body))
      }
  in
    Task.mapError promoteError
      (Http.send Http.defaultSettings request)
        `Task.andThen`
          handleResponse (emptyResponseHandler NoContent)
