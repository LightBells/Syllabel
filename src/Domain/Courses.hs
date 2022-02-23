{-# LANGUAGE TemplateHaskell #-}
module Domain.Courses where

import Data.Aeson
import Data.Aeson.TH

data Course = Course{
    _id         :: Maybe String
  , name        :: String
  , quarter     :: String
  , for         :: String
  , credits     :: String
  , coordinator :: String
  , instructor  :: String
  , recommended :: String
  , essential   :: String
  , updatedOn         :: String 
  , outline           :: String 
  , goals             :: String 
  , schedule          :: String 
  , textbook          :: String 
  , criteria          :: String 
  , note              :: String
  , reference         :: String
  } deriving (Show, Eq)

$(deriveJSON defaultOptions{omitNothingFields = True} ''Course)
