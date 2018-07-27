{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

import           Servant.API  ((:<|>), (:>), Capture, Get, JSON, Post, ReqBody)
import           Servant.Elm  (DefineElm (DefineElm), ElmOptions(urlPrefix),
                               Proxy (Proxy), UrlPrefix(Static), defaultOptions,
                               defElmImports, defElmOptions, deriveBoth,
                               generateElmModuleWith)

data Book = Book
  { name :: String
  } deriving (Show, Eq)

deriveBoth defaultOptions ''Book

type BooksApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
           :<|> "books" :> Get '[JSON] [Book]
           :<|> "books" :> Capture "bookId" Int :> Get '[JSON] Book

myElmOpts :: ElmOptions
myElmOpts = defElmOptions { urlPrefix = Static "http://localhost:8000" }

main :: IO ()
main =
  generateElmModuleWith
    myElmOpts
    [ "Generated"
    , "BooksApi"
    ]
    defElmImports
    "elm"
    [ DefineElm (Proxy :: Proxy Book)
    ]
    (Proxy :: Proxy BooksApi)
