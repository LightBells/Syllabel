{-# LANGUAGE OverloadedStrings #-}
module Repository.Courses where

import Domain.Courses
import Util.BSUtils

import Text.HTML.Scalpel
import Control.Applicative
import Control.Monad
import Network.HTTP.Simple 
import Text.HTML.Scalpel
import qualified Data.ByteString as BS

nItems :: Int -> Scraper s [a] -> Scraper s [a]
nItems n = mfilter ((==n) . length)

fundamentalInfoBuilder courseName columns = CourseFundamentalInfo {
      name        = decodeByteString courseName
    , quarter     = decodeByteString $ columns !! 1
    , for         = decodeByteString $ columns !! 2
    , credits     = decodeByteString $ columns !! 3
    , coordinator = decodeByteString $ columns !! 4
    , instructor  = decodeByteString $ columns !! 5
    , recommended = decodeByteString $ columns !! 6
    , prereq      = decodeByteString $ columns !! 7
    , language    = decodeByteString $ columns !! 8
    , updatedOn         = decodeByteString $ columns !! 9
    , outline           = decodeByteString $ columns !! 10
    , goals             = decodeByteString $ columns !! 11
    , schedule          = decodeByteString $ columns !! 12
    , textbook          = decodeByteString $ columns !! 13
    , criteria          = decodeByteString $ columns !! 14
}

courses :: Scraper BS.ByteString [Course]
courses = chroots ("div" @: [hasClass "sytab"]) course 

course :: Scraper BS.ByteString Course
course = textCourse <|> textCourseWONote <|> textCourseWONoteAndReference

textCourse :: Scraper BS.ByteString Course
textCourse = do 
  courseName <- head <$> texts "li"
  cols <- nItems 17 . texts $ "td"
  return Course {
    fundamentalInfo   = fundamentalInfoBuilder courseName cols
  , note              = decodeByteString $ cols !! 15
  , reference         = decodeByteString $ cols !! 16
  }                   

textCourseWONote :: Scraper BS.ByteString Course
textCourseWONote = do 
  courseName <- head <$> texts "li"
  cols <- nItems 16 . texts $ "td"
  return CourseWONote {
    fundamentalInfo   = fundamentalInfoBuilder courseName cols
  , reference         = decodeByteString $ cols !! 15
  } 

textCourseWONoteAndReference :: Scraper BS.ByteString Course
textCourseWONoteAndReference = do 
  courseName <- head <$> texts "li"
  cols <- nItems 15 . texts $ "td"
  return (CourseWONoteAndReference $ fundamentalInfoBuilder courseName cols)
