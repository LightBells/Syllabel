{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe

import Text.Show.Unicode
import Text.HTML.Scalpel
import Text.Printf
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS

import Control.Concurrent

import Domain.Courses
import Repository.Courses
import Repository.Client

main :: IO ()         
main = do             
  let loop n | n <= 21 = do
        let url = printf "http://web-ext.u-aizu.ac.jp/official/curriculum/syllabus/2022_1_J_0%02d.html" $ (n :: Int)
        parsedText <- scrapeURL url courses
        let result = fromMaybe [] parsedText
        -- ここでresultについての処理を入れる.
        let ids = map ((fromMaybe "") . _id) result
        let jsons =  map (encode . \x -> x{_id = Nothing}) $ result
        let pairOfIdAndJson = zip ids jsons
        
        result <- putCourseInformation pairOfIdAndJson
        print result
        -- mapM_ LBS.putStrLn jsons
        threadDelay (1*1000*1000)
        loop $ n+1
      loop _ = return ()
  loop 1
  HTTPResult statusCode body <- getRequest "http://localhost:8000/v1/commit"
  print statusCode
