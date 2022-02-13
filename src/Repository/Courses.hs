{-# LANGUAGE OverloadedStrings #-}
module Repository.Courses where

import Domain.Courses
import Util.BSUtils
import Util.Strip

import Text.HTML.Scalpel
import Control.Applicative
import Control.Monad
import Network.HTTP.Simple 
import Text.HTML.Scalpel
import qualified Data.ByteString as BS

nItems :: Int -> Scraper s [a] -> Scraper s [a]
nItems n = mfilter ((==n) . length)

fundamentalInfoBuilder courseName columns = Course {
      name        = strip $ decodeByteString courseName
    , quarter     = strip $ decodeByteString $ columns !! 1
    , for         = strip $ decodeByteString $ columns !! 2
    , credits     = strip $ decodeByteString $ columns !! 3
    , coordinator = strip $ decodeByteString $ columns !! 4
    , instructor  = strip $ decodeByteString $ columns !! 5
    , recommended = strip $ decodeByteString $ columns !! 6
    , essential   = strip $ decodeByteString $ columns !! 7
    , updatedOn         = strip $ decodeByteString $ columns !! 8
    , outline           = strip $ decodeByteString $ columns !! 9
    , goals             = strip $ decodeByteString $ columns !! 10
    , schedule          = strip $ decodeByteString $ columns !! 11
    , textbook          = strip $ decodeByteString $ columns !! 12
    , criteria          = strip $ decodeByteString $ columns !! 13
    , note              = strip $ decodeByteString $ columns !! 14
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
     reference              = strip $ decodeByteString $ cols !! 15
  }                   

textCourseWOReference:: Scraper BS.ByteString Course
textCourseWOReference = do 
  courseName <- head <$> texts "li"
  cols <- nItems 15 . texts $ "td"
  return $ fundamentalInfoBuilder courseName cols
