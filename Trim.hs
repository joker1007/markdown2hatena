module Trim
  (ltrim, rtrim, trim) where

  import Data.Char

  ltrim :: String -> String
  ltrim "" = ""
  ltrim (x:xs) | isSpace x = ltrim xs
             | otherwise = (x:xs)

  rtrim :: String -> String
  rtrim = reverse . ltrim . reverse

  trim :: String -> String
  trim = ltrim . rtrim

