decodeBook : Json.Decode.Decoder Book
decodeBook =
  Json.Decode.succeed Book
    |: ("title" := Json.Decode.string)
