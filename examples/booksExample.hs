{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Proxy
import           Elm          (Spec (Spec), ToElmType, specsToDir)
import           GHC.Generics (Generic)
import           Servant      ((:<|>), (:>), Capture, Get, JSON, Post, ReqBody)
import           Servant.Elm  (ElmOptions (..), defElmImports, defElmOptions,
                               generateElmForAPIWith)

data Book = Book
  { name :: String
  } deriving (Show, Eq, Generic)

instance ToElmType Book

type BooksApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
           :<|> "books" :> Get '[JSON] [Book]
           :<|> "books" :> Capture "bookId" Int :> Get '[JSON] Book

myElmOpts :: ElmOptions
myElmOpts = defElmOptions { urlPrefix = "http://localhost:8000" }

spec :: Spec
spec = Spec ["Generated", "BooksApi"]
            (defElmImports
             : generateElmForAPIWith myElmOpts (Proxy :: Proxy BooksApi))

main :: IO ()
main = specsToDir "elm" [spec]
