{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

import Test.Hspec

import Control.Monad (zipWithM_)
import Data.Aeson (ToJSON)
import Data.Proxy (Proxy(Proxy))
import Elm (ToElmType)
import GHC.Generics (Generic)
import Servant.API
import Servant.Elm


data Book = Book
  { title :: String
  } deriving Generic

instance ToJSON Book
instance ToElmType Book


type TestApi = "one" :> Get '[JSON] Int
          :<|> "two" :> ReqBody '[JSON] String :> Post '[JSON] (Maybe Int)
          :<|> "books" :> Capture "id" Int :> Get '[JSON] Book

main :: IO ()
main = hspec $
  describe "encoding a simple api" $
    it "does it" $ do
      getOneSource     <- readFile "test/getOneSource.txt"
      postTwoSource    <- readFile "test/postTwoSource.txt"
      getBooksBySource <- readFile "test/getBooksBySource.txt"
      bookTypeSource   <- readFile "test/bookTypeSource.txt"
      decodeBookSource <- readFile "test/decodeBookSource.txt"
      let generated = map (++ "\n") (generateElmForAPI (Proxy :: Proxy TestApi))
          expected  = [ getOneSource
                      , postTwoSource
                      , bookTypeSource
                      , decodeBookSource
                      , getBooksBySource ]
      zipWithM_ shouldBe generated expected
