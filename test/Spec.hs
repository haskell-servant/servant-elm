{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import           Test.Hspec

import           Control.Monad (zipWithM_)
import           Data.Aeson    (ToJSON)
import           Data.Default  (Default, def)
import           GHC.Generics  (Generic)
import           Servant.API
import           Servant.Elm


data Book = Book
  { title :: String
  } deriving Generic

instance ToJSON Book
instance ToElmType Book


type TestApi =
       "one"     :> Get '[JSON] Int
  :<|> "two"     :> ReqBody '[JSON] String
                 :> Post '[JSON] (Maybe Int)
  :<|> "books"   :> Capture "id" Int
                 :> Get '[JSON] Book
  :<|> "books"   :> QueryFlag "published"
                 :> QueryParam "sort" String
                 :> QueryParams "filters" (Maybe Bool)
                 :> Get '[JSON] [Book]
  :<|> "nothing" :> GetNoContent '[JSON] NoContent

main :: IO ()
main = hspec $
  describe "encoding a simple api" $
    it "does it" $ do
      expected <- mapM readFile
        [ "test/getOneSource.elm"
        , "test/postTwoSource.elm"
        , "test/bookTypeSource.elm"
        , "test/decodeBookSource.elm"
        , "test/getBooksByIdSource.elm"
        , "test/getBooksSource.elm"
        , "test/noContentTypeSource.elm"
        , "test/emptyResponseHandlerSource.elm"
        , "test/handleResponseSource.elm"
        , "test/promoteErrorSource.elm"
        , "test/getNothingSource.elm"
        ]

      let generated = map (++ "\n") (generateElmForAPI (Proxy :: Proxy TestApi))

      generated `itemsShouldBe` expected

itemsShouldBe :: (Default a, Eq a, Show a) => [a] -> [a] -> IO ()
itemsShouldBe actual expected =
  zipWithM_ shouldBe (actual   ++ replicate (length expected - length actual) def)
                     (expected ++ replicate (length actual - length expected) def)
