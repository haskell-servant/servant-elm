{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import           GHC.Generics (Generic)
import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (ElmType, Proxy (Proxy), Spec (Spec),
                               defElmImports, generateElmForAPI, specsToDir,
                               specsToDir)

data Book = Book
  { name :: String
  } deriving (Generic)

instance ElmType Book

type BooksApi = "books" :> Capture "bookId" Int :> Get '[JSON] Book

spec :: Spec
spec = Spec ["Generated", "MyApi"]
            (defElmImports
             : generateElmForAPI (Proxy :: Proxy BooksApi))

main :: IO ()
main = specsToDir [spec] "my-elm-dir"
