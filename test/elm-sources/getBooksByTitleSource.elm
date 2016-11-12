getBooksByTitle : String -> Task.Task Http.Error (Book)
getBooksByTitle title =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          String.join "/"
            [ ""
            , "books"
            , title |> Http.uriEncode
            ]
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeBook
      (Http.send Http.defaultSettings request)
