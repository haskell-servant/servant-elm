{-# LANGUAGE TemplateHaskell #-}

module PolymorphicData where

import Servant.Elm


data PolymorphicData a b = PolymorphicData a b deriving (Show, Eq)
data SomeRecord = SomeRecord
  { recordId :: Int
  , recordName :: String
  } deriving (Show, Eq)

deriveBoth defaultOptions ''PolymorphicData
deriveBoth defaultOptions ''SomeRecord
