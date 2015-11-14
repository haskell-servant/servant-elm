{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import           Data.Proxy
import           Elm               (ToElmType, Spec(Spec), specsToDir)
import           GHC.Generics      (Generic)
import           Servant.API
import           Servant.ElmExport

data Book = Book
  { name :: String
  } deriving (Show, Eq, Generic)

instance ToElmType Book

type TestApi = "hello" :> "world" :> Get '[JSON] Book
          :<|> "change" :> Post '[JSON] Book


spec :: Spec
spec = Spec ["Generated", "Module"]
            (generateElmForAPI (Proxy :: Proxy TestApi))

main :: IO ()
main = specsToDir "elm" [spec]
