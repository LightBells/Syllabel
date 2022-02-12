module Domain.Courses where

data Course = Course{
    name        :: String
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
