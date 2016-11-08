emptyResponseHandler : a -> String -> Task.Task Http.Error a
emptyResponseHandler x str =
  if String.isEmpty str then
    Task.succeed x
  else
    Task.fail (Http.UnexpectedPayload str)
