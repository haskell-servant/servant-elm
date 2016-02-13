 {-# LANGUAGE DeriveGeneric     #-}
 {-# LANGUAGE DataKinds         #-}
 {-# LANGUAGE TypeOperators     #-}

 import           Data.Proxy   (Proxy(Proxy))
 import           Elm          (Spec(Spec), ToElmType, specsToDir)
 import           GHC.Generics (Generic)
 import           Servant.API  ((:>), Capture, Get, JSON)
 import           Servant.Elm  (defElmImports, generateElmForAPI)

 data Book = Book
   { name :: String
   } deriving (Generic)

 instance ToElmType Book

 type BooksApi = "books" :> Capture "bookId" Int :> Get '[JSON] Book

 spec :: Spec
 spec = Spec ["Generated", "MyApi"]
             (defElmImports
              : generateElmForAPI (Proxy :: Proxy BooksApi))

 main :: IO ()
 main = specsToDir [spec] "my-elm-dir"
