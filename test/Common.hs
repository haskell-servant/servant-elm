{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Common where

import           Data.Proxy   (Proxy (Proxy))
import           Data.Text    (Text)
import           Servant.API  ((:<|>), (:>), Capture, Get, GetNoContent, Header,
                               Header', Headers, JSON, Post,
                               PostNoContent, Put, QueryFlag, QueryParam,
                               QueryParam', QueryParams, ReqBody, Required)
import           Servant.Elm  (deriveBoth, defaultOptions)

data Book = Book
    { title :: String
    }

deriveBoth defaultOptions ''Book

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
         :> QueryParam' '[Required] "category" String
         :> QueryParams "filters" (Maybe Bool)
         :> Get '[JSON] [Book]
  :<|> "books"
         :> ReqBody '[JSON] Book
         :> PostNoContent '[JSON] ()
  :<|> "nothing"
         :> GetNoContent '[JSON] ()
  :<|> "nothing"
         :> Put '[JSON] () -- old way to specify no content
  :<|> "with-a-header"
         :> Header "Cookie" String
         :> Header "myStringHeader" String
         :> Header "MyIntHeader" Int
         :> Header' '[Required] "MyRequiredStringHeader" String
         :> Header' '[Required] "MyRequiredIntHeader" Int
         :> Get '[JSON] String
  :<|> "with-a-response-header"
         :> Get '[JSON] (Headers '[Header "myResponse" String] String)

testApi :: Proxy TestApi
testApi = Proxy
