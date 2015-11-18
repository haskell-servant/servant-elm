{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

import Test.Hspec

import Data.Proxy (Proxy(Proxy))
import Servant.API
import Servant.Elm


type TestApi = "one" :> Get '[JSON] Int
          :<|> "two" :> ReqBody '[JSON] String :> Post '[JSON] (Maybe Int)

main :: IO ()
main = hspec $
  describe "encoding a simple api" $
    it "does it" $ do
      getOneSource <- readFile "test/getOneSource.txt"
      postTwoSource <- readFile "test/postTwoSource.txt"
      map (++ "\n") (generateElmForAPI (Proxy :: Proxy TestApi)) `shouldBe` [getOneSource, postTwoSource]
