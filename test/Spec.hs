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
instance ElmType Book


type TestApi =
       "one"     :> Get '[JSON] Int
  :<|> "two"     :> ReqBody '[JSON] String
                 :> Post '[JSON] (Maybe Int)
  :<|> "books"   :> Capture "id" Int
                 :> Get '[JSON] Book
  :<|> "books"   :> Capture "title" String
                 :> Get '[JSON] Book
  :<|> "books"   :> QueryFlag "published"
                 :> QueryParam "sort" String
                 :> QueryParam "year" Int
                 :> QueryParams "filters" (Maybe Bool)
                 :> Get '[JSON] [Book]
  :<|> "books"   :> ReqBody '[JSON] Book
                 :> PostNoContent '[JSON] NoContent
  :<|> "nothing" :> GetNoContent '[JSON] NoContent
  :<|> "nothing" :> Put '[JSON] () -- old way to specify no content

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
        , "test/getBooksByTitleSource.elm"
        , "test/getBooksSource.elm"
        , "test/noContentTypeSource.elm"
        , "test/encodeBookSource.elm"
        , "test/emptyResponseHandlerSource.elm"
        , "test/handleResponseSource.elm"
        , "test/promoteErrorSource.elm"
        , "test/postBooksSource.elm"
        , "test/getNothingSource.elm"
        , "test/putNothingSource.elm"
        ]

      let generated = map (++ "\n") (generateElmForAPI (Proxy :: Proxy TestApi))

      generated `itemsShouldBe` expected

itemsShouldBe :: (Default a, Eq a, Show a) => [a] -> [a] -> IO ()
itemsShouldBe actual expected =
  zipWithM_ shouldBe (actual   ++ replicate (length expected - length actual) def)
                     (expected ++ replicate (length actual - length expected) def)
