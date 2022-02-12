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

fundamentalInfoBuilder courseName columns = Course {
      name        = decodeByteString courseName
    , quarter     = decodeByteString $ columns !! 1
    , for         = decodeByteString $ columns !! 2
    , credits     = decodeByteString $ columns !! 3
    , coordinator = decodeByteString $ columns !! 4
    , instructor  = decodeByteString $ columns !! 5
    , recommended = decodeByteString $ columns !! 6
    , essential   = decodeByteString $ columns !! 7
    , updatedOn         = decodeByteString $ columns !! 8
    , outline           = decodeByteString $ columns !! 9
    , goals             = decodeByteString $ columns !! 10
    , schedule          = decodeByteString $ columns !! 11
    , textbook          = decodeByteString $ columns !! 12
    , criteria          = decodeByteString $ columns !! 13
    , note              = decodeByteString $ columns !! 14
    , reference         = ""
}

courses :: Scraper BS.ByteString [Course]
courses = chroots ("div" @: [hasClass "sytab"]) course 

course :: Scraper BS.ByteString Course
course = textCourse <|> textCourseWOReference

textCourse:: Scraper BS.ByteString Course
textCourse= do 
  courseName <- head <$> texts "li"
  cols <- nItems 16 . texts $ "td"
  let courseWithFundamentalInfo = fundamentalInfoBuilder courseName cols
  return courseWithFundamentalInfo {
     reference              = decodeByteString $ cols !! 15
  }                   

textCourseWOReference:: Scraper BS.ByteString Course
textCourseWOReference = do 
  courseName <- head <$> texts "li"
  cols <- nItems 15 . texts $ "td"
  return $ fundamentalInfoBuilder courseName cols
