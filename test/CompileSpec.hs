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
                      "avh4/elm-program-test": "3.2.0",
                      "elm/core": "1.0.2",
                      "elm/json": "1.1.2",
                      "elm/http": "2.0.0",
                      "elm/url": "1.0.0",
                      "NoRedInk/elm-string-conversions": "1.0.1",
                      "NoRedInk/elm-json-decode-pipeline": "1.0.0"
                  },
                  "indirect": {
                    "avh4/elm-fifo": "1.0.4",
                    "elm/browser": "1.0.2",
                    "elm/bytes": "1.0.7",
                    "elm/file": "1.0.1",
                    "elm/html": "1.0.0",
                    "elm/random": "1.0.0",
                    "elm/virtual-dom": "1.0.2",
                    "elm/time": "1.0.0",
                    "elm-explorations/test": "1.2.2"
                  }
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
