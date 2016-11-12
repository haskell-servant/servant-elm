getNothing : Task.Task Http.Error (NoContent)
getNothing =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          String.join "/"
            [ ""
            , "nothing"
            ]
      , body =
          Http.empty
      }
  in
    Task.mapError promoteError
      (Http.send Http.defaultSettings request)
        `Task.andThen`
          handleResponse (emptyResponseHandler NoContent)
