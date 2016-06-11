{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TypeOperators #-}

import           Test.Hspec
import           Test.Mockery.Directory

import           Control.Exception
import           Control.Monad (when, zipWithM_)
import           Data.Aeson    (ToJSON)
import           Data.Default  (Default, def)
import           Data.List
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           GHC.Generics  (Generic)
import           Servant.API
import           Servant.Elm
import           System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , getCurrentDirectory
  , removeFile
  , setCurrentDirectory
  )
import           System.Process


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

testApi :: Proxy TestApi
testApi = Proxy

main :: IO ()
main = hspec $ do
  describe "encoding a simple api" $ do
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

      let generated = map (++ "\n") (generateElmForAPI testApi)

      generated `itemsShouldBe` expected

    it "creates compilable javascript" $ do
      inTempElmDir $ do
        let generated =
              intercalate "\n\n" $
                defElmImports :
                generateElmForAPI testApi
        writeFile "Api.elm" generated
        callCommand "elm-make Api.elm --output api.js"

itemsShouldBe :: (Default a, Eq a, Show a) => [a] -> [a] -> IO ()
itemsShouldBe actual expected =
  zipWithM_ shouldBe (actual   ++ replicate (length expected - length actual) def)
                     (expected ++ replicate (length actual - length expected) def)

inTempElmDir :: IO a -> IO a
inTempElmDir action = do
  cacheExists <- doesDirectoryExist "_test-cache"
  when (not cacheExists)
    createCache
  cacheDir <- canonicalizePath "_test-cache"
  inTempDirectory $ do
    callCommand ("cp -r " ++ cacheDir ++ "/* .")
    action

createCache :: IO ()
createCache = do
  createDirectoryIfMissing True "_test-cache"
  withCurrentDirectory "_test-cache" $ do
    writeFile "elm-package.json" $ unindent $ [i|
      {
          "version": "1.0.0",
          "summary": "helpful summary of your project, less than 80 characters",
          "repository": "https://github.com/user/project.git",
          "license": "BSD3",
          "source-directories": [
              "."
          ],
          "exposed-modules": [],
          "dependencies": {
              "elm-lang/core": "4.0.1 <= v < 5.0.0",
              "elm-community/elm-json-extra": "1.0.1 <= v < 2.0.0",
              "evancz/elm-http": "3.0.1 <= v < 4.0.0"
          },
          "elm-version": "0.17.0 <= v < 0.18.0"
      }
    |]
    callCommand "elm-package install --yes"
    compileDependencies

compileDependencies :: IO ()
compileDependencies = do
  writeFile "Main.elm" "foo = 42"
  callCommand "elm-make Main.elm --output main.js"
  removeFile "Main.elm"
  removeFile "main.js"

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action =
  bracket enter recover (const action)
  where
    enter = do
      original <- getCurrentDirectory
      setCurrentDirectory dir
      return original
    recover = setCurrentDirectory
