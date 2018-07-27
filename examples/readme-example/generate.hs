{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (DefineElm (DefineElm), Proxy (Proxy),
                               defaultOptions, defElmImports, defElmOptions,
                               deriveBoth, generateElmModuleWith)

data Book = Book
  { name :: String
  }

deriveBoth defaultOptions ''Book

type BooksApi = "books" :> Capture "bookId" Int :> Get '[JSON] Book

main :: IO ()
main =
  generateElmModuleWith
    defElmOptions
    [ "Generated"
    , "MyApi"
    ]
    defElmImports
    "my-elm-dir"
    [ DefineElm (Proxy :: Proxy Book)
    ]
    (Proxy :: Proxy BooksApi)
