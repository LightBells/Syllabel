module Util.Strip where

lstrip [] = []
lstrip xs'@(x:xs) | x == ' ' = lstrip xs
                  | x == '\r' = lstrip xs
                  | x == '\n' = lstrip xs
                  | otherwise = xs'

rstrip = reverse . lstrip . reverse

strip = lstrip . rstrip
