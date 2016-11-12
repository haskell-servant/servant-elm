getBooksById : Int -> Task.Task Http.Error (Book)
getBooksById id =
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
            , id |> toString |> Http.uriEncode
            ]
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeBook
      (Http.send Http.defaultSettings request)
