{-# LANGUAGE OverloadedStrings #-}
module Repository.Courses where

import Domain.Courses
import Util.BSUtils
import Util.Strip

import Data.List.Split
import Text.HTML.Scalpel
import Control.Applicative
import Control.Monad
import Network.HTTP.Simple 
import Text.HTML.Scalpel
import qualified Data.ByteString as BS

nItems :: Int -> Scraper s [a] -> Scraper s [a]
nItems n = mfilter ((==n) . length)

preprocess :: BS.ByteString -> String
preprocess = strip . decodeByteString

fundamentalInfoBuilder courseName columns = Course {
      _id               = (splitOn " " $ preprocess courseName) !! 0
    , name              = preprocess courseName
    , quarter           = preprocess $ columns !! 1
    , for               = preprocess $ columns !! 2
    , credits           = preprocess $ columns !! 3
    , coordinator       = preprocess $ columns !! 4
    , instructor        = preprocess $ columns !! 5
    , recommended       = preprocess $ columns !! 6
    , essential         = preprocess $ columns !! 7
    , updatedOn         = preprocess $ columns !! 8
    , outline           = preprocess $ columns !! 9
    , goals             = preprocess $ columns !! 10
    , schedule          = preprocess $ columns !! 11
    , textbook          = preprocess $ columns !! 12
    , criteria          = preprocess $ columns !! 13
    , note              = preprocess $ columns !! 14
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
     reference              = preprocess $ cols !! 15
  }                   

textCourseWOReference:: Scraper BS.ByteString Course
textCourseWOReference = do 
  courseName <- head <$> texts "li"
  cols <- nItems 15 . texts $ "td"
  return $ fundamentalInfoBuilder courseName cols
