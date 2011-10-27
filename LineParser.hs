--
-- $Id: LineParser.hs,v 1.4 2006/04/09 20:40:24 aamine Exp $
--
-- Copyright (c) 2005,2006 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LGPL, see the file "COPYING".
--

module LineParser
  (LineParser, indented, blank, notBlank, lineSatisfy, firstChar) where

  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Prim
  import Data.Char
  import Trim

  type LineParser a = GenParser String () a

  lineSatisfy :: (String -> Bool) -> LineParser String
  lineSatisfy f = tokenPrim showLine nextPos testLine
    where
      showLine line = show line
      nextPos pos line s = incSourceLine pos 1
      testLine line = if f line then Just line else Nothing

  indented :: LineParser String
  indented = firstChar isSpace

  blank :: LineParser String
  blank = lineSatisfy (null . dropWhile isSpace)

  notBlank :: LineParser String
  notBlank = lineSatisfy ((0 < ) . length . trim)

  firstChar :: (Char -> Bool) -> LineParser String
  firstChar f = lineSatisfy (test f)
    where
      test _ "" = False
      test f (c:_) = f c

