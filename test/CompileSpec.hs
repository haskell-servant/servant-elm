{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Test.Hspec
import           Test.Mockery.Directory

import           Control.Exception
import           Control.Monad                (when)
import           Data.List                    (intercalate)
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Elm.Versions (ElmVersion(..))
import           Servant.Elm
import           System.Directory             (canonicalizePath,
                                               createDirectoryIfMissing,
                                               doesDirectoryExist,
                                               getCurrentDirectory, removeFile,
                                               setCurrentDirectory)
import           System.Process (callCommand)
import           System.Process.Typed
import qualified Data.Text as T
import qualified Elm.Module                   as Elm
import           Data.Text.IO                 as TIO

import Common (Book, TestApi)

main :: IO ()
main =
  hspec spec

spec :: Test.Hspec.Spec
spec = do
  describe "generateElmForAPI" $ do
    it "creates compilable javascript" $ do
      inTempElmDir $ do
        let options = defElmOptions
            namespace :: [String]
            namespace = ["Generated", "BooksApi"]
            imports = defElmImports
            rootDir = "."
            typeDefs = [ DefineElm (Proxy :: Proxy Book)
                       ]
            api = (Proxy :: Proxy TestApi)
        let out =
              T.unlines $
              [ T.pack $ Elm.moduleHeader Elm0p18 moduleName
              , ""
              , imports
              , ""
              , "type NoContent = NoContent"
              , ""
              , T.pack $ Elm.makeModuleContentWithAlterations (elmAlterations options) typeDefs
              ] ++
              generateElmForAPIWith options api
            moduleName = T.unpack (T.intercalate "." (map T.pack namespace))
            filePath = intercalate "/" $ rootDir:init namespace
            fileName = intercalate "/" $ filePath : [last namespace ++ ".elm"]
        createDirectoryIfMissing True filePath
        TIO.writeFile fileName out

        -- -- Useful for locally checking out sources in your tmp dir
        -- callCommand "cp -r . /home/kb/tmp/servelmtest"
        runProcess_ "elm make Generated/BooksApi.elm --output api.js"


inTempElmDir :: IO a -> IO a
inTempElmDir action = do
  callCommand "rm -rf ./_test-cache"
  cacheExists <- doesDirectoryExist "_test-cache"
  when (not cacheExists) createCache
  cacheDir <- canonicalizePath "_test-cache"
  inTempDirectory $ do
    callCommand ("cp -r " ++ cacheDir ++ "/* .")
    action

createCache :: IO ()
createCache = do
  createDirectoryIfMissing True "_test-cache"
  withCurrentDirectory "_test-cache" $ do
    TIO.writeFile "elm.json" $ T.pack $ unindent $ [i|
      {
          "type": "application",
          "source-directories": [
              "."
          ],
          "elm-version": "0.19.0",
          "version": "1.0.0",
          "summary": "helpful summary of your project, less than 80 characters",
          "dependencies": {
              "direct": {
                "elm/core": "1.0.2",
                "elm/json": "1.1.3",
                "elm/http": "2.0.0",
                "elm/url": "1.0.0",
                "bartavelle/json-helpers": "2.0.2"
              },
              "indirect": {
                 "elm/bytes": "1.0.8",
                 "elm/file": "1.0.5",
                 "elm/time": "1.0.0"
              }
          },
          "test-dependencies": {
            "direct": {},
            "indirect": {}
          }
      }
    |]
    -- callCommand "elm install"
    -- compileDependencies

compileDependencies :: IO ()
compileDependencies = do
  TIO.writeFile "Main.elm" "module Main exposing (foo)\n\nfoo : Int\nfoo = 42\n"
  callCommand "echo '>>>>>> Main.elm'"
  callCommand "cat Main.elm"
  callCommand "elm make Main.elm --output main.js"
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
