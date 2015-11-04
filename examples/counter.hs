{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Proxy
import           Servant         ((:<|>), (:>), Get, JSON, Post)
import qualified Servant.JS      as SJS
import           Servant.JS.Elm  (elmJSWith)
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath

type Counter = Int

-- * Our API type
type TestApi = "counter" :> "inc" :> Post '[JSON] Counter -- endpoint for increasing the counter
          :<|> "counter" :> Get '[JSON] Counter -- endpoint to get the current value

testApi :: Proxy TestApi
testApi = Proxy

elmDir :: FilePath
elmDir = "elm"


main :: IO ()
main = do
  let myElmJS =
        elmJSWith $
          SJS.defCommonGeneratorOptions
            { SJS.moduleName = "Generated.Api"
            , SJS.urlPrefix = "http://localhost:8080"
            }
  createDirectoryIfMissing True (elmDir </> "Generated")
  SJS.writeJSForAPI testApi myElmJS (elmDir </> "Generated" </> "Api.elm")
