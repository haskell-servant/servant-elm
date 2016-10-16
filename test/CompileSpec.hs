{-# LANGUAGE QuasiQuotes       #-}

module CompileSpec where

import           Test.Hspec
import           Test.Mockery.Directory

import           Control.Exception
import           Control.Monad                (when)
import           Data.List
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import qualified Data.Text                    as T
import           Elm                          (toElmDecoderSource,
                                               toElmEncoderSource,
                                               toElmTypeSource)
import           GenerateSpec
import           Servant.API                  (NoContent)
import           Servant.Elm
import           System.Directory             (canonicalizePath,
                                               createDirectoryIfMissing,
                                               doesDirectoryExist,
                                               getCurrentDirectory, removeFile,
                                               setCurrentDirectory)
import           System.Process

spec :: Test.Hspec.Spec
spec = do
  describe "generateElmForAPI" $ do
    it "creates compilable javascript" $ do
      inTempElmDir $ do
        let generated =
              intercalate "\n\n" $
                defElmImports :
                [ T.unpack $ toElmTypeSource (Proxy :: Proxy NoContent)
                , T.unpack $ toElmTypeSource (Proxy :: Proxy Book)
                , T.unpack $ toElmDecoderSource (Proxy :: Proxy Book)
                , T.unpack $ toElmEncoderSource (Proxy :: Proxy Book)
                ] ++
                generateElmForAPI testApi
        writeFile "Api.elm" generated
        callCommand "elm-make Api.elm --output api.js"

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
              "elm-community/json-extra": "1.0.0 <= v < 2.0.0",
              "evancz/elm-http": "3.0.1 <= v < 4.0.0",
              "NoRedInk/elm-decode-pipeline": "2.0.0 <= v < 3.0.0"
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
