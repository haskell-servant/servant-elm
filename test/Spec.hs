{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

import Test.Hspec

import Control.Monad (zipWithM_)
import Data.Aeson (ToJSON)
import Data.Default (Default, def)
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
          :<|> "books" :> QueryFlag "published"
                       :> QueryParam "sort" String
                       :> QueryParams "filters" (Maybe Bool)
                       :> Get '[JSON] [Book]
          :<|> "nothing" :> Get '[JSON] ()

main :: IO ()
main = hspec $
  describe "encoding a simple api" $
    it "does it" $ do
      getOneSource     <- readFile "test/getOneSource.elm"
      postTwoSource    <- readFile "test/postTwoSource.elm"
      bookTypeSource   <- readFile "test/bookTypeSource.elm"
      decodeBookSource <- readFile "test/decodeBookSource.elm"
      getBooksBySource <- readFile "test/getBooksByIdSource.elm"
      getBooksSource   <- readFile "test/getBooksSource.elm"
      getNothingSource <- readFile "test/getNothingSource.elm"
      let generated = map (++ "\n") (generateElmForAPI (Proxy :: Proxy TestApi))
          expected  = [ getOneSource
                      , postTwoSource
                      , bookTypeSource
                      , decodeBookSource
                      , getBooksBySource
                      , getBooksSource
                      , getNothingSource
                      ]
      generated `itemsShouldBe` expected

itemsShouldBe :: (Default a, Eq a, Show a) => [a] -> [a] -> IO ()
itemsShouldBe actual expected =
  zipWithM_ shouldBe (actual   ++ replicate (length expected - length actual) def)
                     (expected ++ replicate (length actual - length expected) def)
