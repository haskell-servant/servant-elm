{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Test.Hspec
import           Test.Mockery.Directory

import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Elm                          (toElmDecoderSource,
                                               toElmEncoderSource,
                                               toElmTypeSource)
import           Servant.API                  (NoContent)
import           Servant.Elm
import           System.Process

import Common (Book, testApi)

main :: IO ()
main =
  hspec spec

spec :: Test.Hspec.Spec
spec = do
  describe "generateElmForAPI" $ do
    it "creates compilable javascript" $ do
      inTempDirectory $ do
        writeFile "elm.json" $ unindent $ [i|
          {
              "type": "application",
              "source-directories": [
                  "."
              ],
              "elm-version": "0.19.0",
              "dependencies": {
                  "direct": {
                      "elm/core": "1.0.0",
                      "elm/json": "1.0.0",
                      "elm-community/json-extra": "3.0.0",
                      "elm/http": "1.0.0",
                      "NoRedInk/elm-json-decode-pipeline": "1.0.0"
                  },
                  "indirect": {}
              },
              "test-dependencies": {
                  "direct": {},
                  "indirect": {}
              }
          }
        |]
        let generated =
              T.intercalate "\n\n" $
                defElmImports :
                [ toElmTypeSource (Proxy :: Proxy NoContent)
                , toElmTypeSource (Proxy :: Proxy Book)
                , toElmDecoderSource (Proxy :: Proxy Book)
                , toElmEncoderSource (Proxy :: Proxy Book)
                ] ++
                generateElmForAPI testApi
        T.writeFile "Api.elm" generated
        callCommand "elm make Api.elm --output api.js"
