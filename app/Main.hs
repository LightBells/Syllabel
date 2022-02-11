module Main where

import Data.Maybe

import Text.Show.Unicode
import Text.HTML.Scalpel

import Domain.Courses
import Repository.Courses

main :: IO ()         
main = do             
  let url = "http://web-ext.u-aizu.ac.jp/official/curriculum/syllabus/2022_1_J_002.html"
  parsedText <- scrapeURL url courses
  let result = fromMaybe [] parsedText
  mapM_ (putStrLn . ushow) result
