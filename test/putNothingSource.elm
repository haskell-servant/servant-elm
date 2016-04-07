putNothing : Task.Task Http.Error (())
putNothing =
  let
    request =
      { verb =
          "PUT"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "nothing"
      , body =
          Http.empty
      }
  in
    Task.mapError promoteError
      (Http.send Http.defaultSettings request)
        `Task.andThen`
          handleResponse (emptyResponseHandler ())
