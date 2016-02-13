postTwo : String -> Task.Task Http.Error (Maybe Int)
postTwo body =
  let request =
        { verb = "POST"
        , headers = [("Content-Type", "application/json")]
        , url = "/" ++ "two"
        , body = Http.string (Json.Encode.encode 0 (Json.Encode.string body))
        }
  in  Http.fromJson
        (Json.Decode.maybe Json.Decode.int)
        (Http.send Http.defaultSettings request)
