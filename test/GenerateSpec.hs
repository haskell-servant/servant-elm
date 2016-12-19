{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module GenerateSpec where

import           Control.Monad             (zipWithM_)
import           Data.Aeson                (ToJSON)
import qualified Data.Algorithm.Diff       as Diff
import qualified Data.Algorithm.DiffOutput as Diff
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           GHC.Generics              (Generic)
import           Servant.API
import           Servant.Elm
import           Test.Hspec                (Spec, describe, it)
import           Test.HUnit                (Assertion, assertBool)


data Book = Book
    { title :: String
    } deriving (Generic)

instance ToJSON Book
instance ElmType Book


type TestApi =
        "one"           :> Get '[JSON] Int
    :<|> "two"           :> ReqBody '[JSON] String
                        :> Post '[JSON] (Maybe Int)
    :<|> "books"         :> Capture "id" Int
                        :> Get '[JSON] Book
    :<|> "books"         :> Capture "title" String
                        :> Get '[JSON] Book
    :<|> "books"         :> QueryFlag "published"
                        :> QueryParam "sort" String
                        :> QueryParam "year" Int
                        :> QueryParams "filters" (Maybe Bool)
                        :> Get '[JSON] [Book]
    :<|> "books"         :> ReqBody '[JSON] Book
                        :> PostNoContent '[JSON] NoContent
    :<|> "nothing"       :> GetNoContent '[JSON] NoContent
    :<|> "nothing"       :> Put '[JSON] () -- old way to specify no content
    :<|> "with-a-header" :> Header "myStringHeader" String
                        :> Header "myIntHeader" Int
                        :> Get '[JSON] String

testApi :: Proxy TestApi
testApi = Proxy

spec :: Test.Hspec.Spec
spec = do
    describe "encoding a simple api" $
        do it "does it" $
               do expected <-
                      mapM
                          (\(fpath,header) -> do
                               source <- T.readFile fpath
                               return (fpath, header, source))
                          [ ( "test/elm-sources/getOneSource.elm"
                            , "module GetOneSource exposing (..)\n\n" <>
                              "import Http\n" <>
                              "import Json.Decode exposing (..)\n\n\n")
                          , ( "test/elm-sources/postTwoSource.elm"
                            , "module PostTwoSource exposing (..)\n\n" <>
                              "import Http\n" <>
                              "import Json.Decode exposing (..)\n" <>
                              "import Json.Encode\n\n\n")
                          , ( "test/elm-sources/getBooksByIdSource.elm"
                            , "module GetBooksByIdSource exposing (..)\n\n" <>
                              "import Http\n\n\n")
                          , ( "test/elm-sources/getBooksByTitleSource.elm"
                            , "module GetBooksByTitleSource exposing (..)\n\n" <>
                              "import Http\n\n\n")
                          , ( "test/elm-sources/getBooksSource.elm"
                            , "module GetBooksSource exposing (..)\n\n" <>
                              "import Http\n" <>
                              "import Json.Decode exposing (..)\n\n\n")
                          , ( "test/elm-sources/postBooksSource.elm"
                            , "module PostBooksSource exposing (..)\n\n" <>
                              "import Http\n\n\n")
                          , ( "test/elm-sources/getNothingSource.elm"
                            , "module GetNothingSource exposing (..)\n\n" <>
                              "import Http\n\n\n")
                          , ( "test/elm-sources/putNothingSource.elm"
                            , "module PutNothingSource exposing (..)\n\n" <>
                              "import Http\n\n\n")
                          , ( "test/elm-sources/getWithaheaderSource.elm"
                            , "module GetWithAHeaderSource exposing (..)\n\n" <>
                              "import Http\n" <>
                              "import Json.Decode exposing (..)\n\n\n")]
                  let generated = map (<> "\n") (generateElmForAPI testApi)
                  generated `itemsShouldBe` expected

itemsShouldBe :: [Text] -> [(String, Text, Text)] -> IO ()
itemsShouldBe actual expected =
    zipWithM_
        shouldBeDiff
        (actual ++ replicate (length expected - length actual) mempty)
        (expected ++ replicate (length actual - length expected) mempty)

shouldBeDiff :: Text -> (String, Text, Text) -> Assertion
shouldBeDiff a (fpath,header,b) =
    assertBool
        ("< generated\n" <> "> " <> fpath <> "\n" <>
         Diff.ppDiff
             (Diff.getGroupedDiff
                  (lines (T.unpack (header <> a)))
                  (lines (T.unpack b))))
        (header <> a == b)
