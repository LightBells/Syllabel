{-# LANGUAGE OverloadedStrings #-}
module Domain.JSONCourses where

import Domain.Courses

import Data.Aeson
import Control.Applicative

-- TODO: Template Haskellで定義さぼれるらしい. とりあえず明示定義しておいて, 後で直す.
-- これ書くのあほでしょ.

instance FromJSON Course where
  parseJSON (Object course) = Course <$> (course .: "name")
                                     <*> (course .: "quarter")
                                     <*> (course .: "for")
                                     <*> (course .: "credits")
                                     <*> (course .: "coordinator")
                                     <*> (course .: "instructor")
                                     <*> (course .: "recommended")
                                     <*> (course .: "essential")
                                     <*> (course .: "updatedOn")
                                     <*> (course .: "outline")
                                     <*> (course .: "goals")
                                     <*> (course .: "schedule")
                                     <*> (course .: "textbook")
                                     <*> (course .: "criteria")
                                     <*> (course .: "note")
                                     <*> (course .: "reference")

instance ToJSON Course where
  toJSON (Course name quarter for credits
                 coordinator instructor recommended
                 essential updatedOn outline
                 goals schedule textbook criteria
                 note reference
         ) = object [ "name" .= name,
                      "quarter" .= quarter,
                      "for" .= for,
                      "credits" .= credits,
                      "coordinator" .= coordinator,
                      "instructor" .= instructor,
                      "recommended" .= recommended,
                      "essential" .= essential,
                      "updatedOn" .= updatedOn,
                      "outline" .= outline,
                      "goals" .= goals,
                      "schedule" .= schedule,
                      "textbook" .= textbook,
                      "criteria" .= criteria,
                      "note" .= criteria,
                      "reference" .= reference]
