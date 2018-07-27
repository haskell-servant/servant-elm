{-|
Basic usage:

> import MyLib (MyServantApiType)
> import Servant.Elm
>
> spec :: Spec
> spec = Spec ["Generated", "MyApi"]
>             (defElmImports : generateElmForAPI (Proxy :: Proxy MyServantApiType))
>
> main :: IO ()
> main = specsToDir [spec] "my-elm-dir"
-}
module Servant.Elm
       ( generateElmForAPI
       , generateElmForAPIWith
       , generateElmModule
       , generateElmModuleWith
       , ElmOptions(..)
       , UrlPrefix(..)
       , defElmOptions
       , defElmImports
       , defaultOptions
       -- * Convenience re-exports from the "Elm" module
       , DefineElm (..)
       , EType (..)
       , defaultTypeAlterations
       , toElmType
       , deriveBoth
       , deriveElmDef
       -- * Convenience re-exports from "Data.Proxy"
       , Proxy(Proxy)
       ) where

import           Servant.Elm.Internal.Generate (ElmOptions (..), UrlPrefix (..),
                                                defElmImports, defElmOptions,
                                                generateElmForAPI,
                                                generateElmForAPIWith,
                                                generateElmModule,
                                                generateElmModuleWith)
import           Servant.Elm.Internal.Options  (defaultOptions)
import           Data.Proxy                    (Proxy (Proxy))
import           Elm.TyRep                     (EType (..), toElmType)
import           Elm.Module                    (DefineElm (..), defaultTypeAlterations)
import           Elm.Derive                    (deriveBoth, deriveElmDef)
