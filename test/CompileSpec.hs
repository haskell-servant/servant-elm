{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module CompileSpec where

import           Test.Hspec
import           Test.Mockery.Directory

import           Control.Exception
import           Control.Monad (when)
import           Data.List
import           Data.Map
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Elm
import           GHC.Generics
import           GenerateSpec
import           Servant.API
import           Servant.Elm
import           Servant.Elm.Foreign
import           Servant.Foreign.Internal
import           System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , getCurrentDirectory
  , removeFile
  , setCurrentDirectory
  )
import           System.Process

data WithMap
  = WithMap {
    withMapMap :: Map String Int
  }
  deriving (Generic)

instance ElmType WithMap

type MapApi =
  "foo" :> Get '[JSON] WithMap

mapApi :: Proxy MapApi
mapApi = Proxy

spec :: Test.Hspec.Spec
spec = do
  describe "generateElmForAPI" $ do
    it "creates compilable javascript for the TestApi" $ do
      shouldCompile testApi

    it "allows to use Maps" $ do
      shouldCompile mapApi

shouldCompile :: (HasForeign LangElm ElmTypeExpr api, GenerateList ElmTypeExpr (Foreign ElmTypeExpr api)) =>
  Proxy api -> IO ()
shouldCompile proxy = do
  inTempElmDir $ do
    let generated =
          intercalate "\n\n" $
            defElmImports :
            generateElmForAPI proxy
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
