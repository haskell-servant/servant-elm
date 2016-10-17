{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Elm          (Spec (Spec), specsToDir, toElmDecoderSource,
                               toElmTypeSource)
import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (ElmType, Proxy (Proxy), defElmImports,
                               generateElmForAPI)

data Book = Book
  { name :: String
  } deriving (Generic)

instance ElmType Book

type BooksApi = "books" :> Capture "bookId" Int :> Get '[JSON] Book

spec :: Spec
spec = Spec ["Generated", "MyApi"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy Book)
             : toElmDecoderSource (Proxy :: Proxy Book)
             : generateElmForAPI  (Proxy :: Proxy BooksApi))

main :: IO ()
main = specsToDir [spec] "my-elm-dir"
