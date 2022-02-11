{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Network.HTTP.Simple 
import Text.HTML.Scalpel
import Data.Maybe

import Codec.Binary.UTF8.String
import Text.Show.Unicode
import qualified Data.ByteString as BS

nItems :: Int -> Scraper s [a] -> Scraper s [a]
nItems n = mfilter ((==n) . length)

decodeByteString = decode . BS.unpack

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
                      
                      
main :: IO ()         
main = do             
  let url = "http://web-ext.u-aizu.ac.jp/official/curriculum/syllabus/2022_1_J_002.html"
  parsedText <- scrapeURL url courses
  let result = fromMaybe [] parsedText
  mapM_ (putStrLn . ushow) result
