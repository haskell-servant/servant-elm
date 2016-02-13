getOne : Task.Task Http.Error (Int)
getOne =
  let request =
        { verb = "GET"
        , headers = [("Content-Type", "application/json")]
        , url = "/" ++ "one"
        , body = Http.empty
        }
  in  Http.fromJson
        Json.Decode.int
        (Http.send Http.defaultSettings request)
