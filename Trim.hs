--
-- markdown2hatena
--
-- Copyright (c) 2011 Tomohiro Hashidate
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LGPL, see the file "COPYING".
--

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

