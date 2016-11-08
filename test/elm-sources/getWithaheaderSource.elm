getWithaheader : String -> Int -> Task.Task Http.Error (String)
getWithaheader myStringHeader myIntHeader =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")
          ,("myStringHeader", myStringHeader)
          ,("myIntHeader", toString myIntHeader)]
      , url =
          "/" ++ "with-a-header"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      string
      (Http.send Http.defaultSettings request)
