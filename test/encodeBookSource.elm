encodeBook : Book -> Json.Encode.Value
encodeBook x =
  Json.Encode.object
    [ ( "title", Json.Encode.string x.title )
    ]
