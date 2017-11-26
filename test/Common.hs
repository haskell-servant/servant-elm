{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Common where

import           Data.Aeson   (ToJSON)
import           Data.Proxy   (Proxy (Proxy))
import           Data.Text    (Text)
import           Elm          (ElmType)
import           GHC.Generics (Generic)
import           Servant.API  ((:<|>), (:>), Capture, Get, GetNoContent, Header,
                               Headers, JSON, NoContent, Post, PostNoContent,
                               Put, QueryFlag, QueryParam, QueryParams, ReqBody)

data Book = Book
    { title :: String
    } deriving (Generic)

instance ToJSON Book
instance ElmType Book

type TestApi =
       "one"
         :> Get '[JSON] Int
  :<|> "two"
         :> ReqBody '[JSON] String
         :> Post '[JSON] (Maybe Int)
  :<|> "books"
         :> Capture "id" Int
         :> Get '[JSON] Book
  :<|> "books"
         :> Capture "title" Text
         :> Get '[JSON] Book
  :<|> "books"
         :> QueryFlag "published"
         :> QueryParam "sort" String
         :> QueryParam "year" Int
         :> QueryParams "filters" (Maybe Bool)
         :> Get '[JSON] [Book]
  :<|> "books"
         :> ReqBody '[JSON] Book
         :> PostNoContent '[JSON] NoContent
  :<|> "nothing"
         :> GetNoContent '[JSON] NoContent
  :<|> "nothing"
         :> Put '[JSON] () -- old way to specify no content
  :<|> "with-a-header"
         :> Header "myStringHeader" String
         :> Header "MyIntHeader" Int
         :> Get '[JSON] String
  :<|> "with-a-response-header"
         :> Get '[JSON] (Headers '[Header "myResponse" String] String)

testApi :: Proxy TestApi
testApi = Proxy
