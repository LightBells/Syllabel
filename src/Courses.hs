module Courses where

import Text.HTML.Scalpel

data CourseFundamentalInfo = CourseFundamentalInfo {
    name        :: String
  , quarter     :: String
  , for         :: String
  , credits     :: String
  , coordinator :: String
  , instructor  :: String
  , recommended :: String
  , prereq      :: String
  , language    :: String
  , updatedOn         :: String 
  , outline           :: String 
  , goals             :: String 
  , schedule          :: String 
  , textbook          :: String 
  , criteria          :: String 
  } deriving (Show, Eq)

data Course = Course{
    fundamentalInfo   :: CourseFundamentalInfo
  , note              :: String
  , reference         :: String
  } 
  | CourseWONote {
    fundamentalInfo   :: CourseFundamentalInfo
  , reference         :: String
  }
  | CourseWONoteAndReference CourseFundamentalInfo
  deriving (Eq, Show)
