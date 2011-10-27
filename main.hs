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

import System
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec
import LineParser
import Trim
import Node

headline :: LineParser [Node]
headline = do line <- firstChar (== '#')
              let (mark, str) = span (== '#') line
              return $ [HeadLine (length mark) (trim str)]

listblock :: LineParser [Node]
listblock = do manyLines <- many1 (lineSatisfy isListLine)
               return $ map listParse manyLines
            where
              listParse :: String -> Node
              listParse cs = case runParser levelParser 1 "" cs of
                                  Right node -> node
                                  Left err -> Paragraph $ show err
              levelParser :: GenParser Char Int Node
              levelParser = do try (count 4 space)
                               updateState (+1)
                               levelParser
                            <|> do tab
                                   updateState (+1)
                                   levelParser
                            <|> do listmark
                                   cs'' <- many anyChar
                                   level' <- getState
                                   return $ ListLine level' (trim cs'')
                            <|> do numberedlistmark
                                   cs'' <- many anyChar
                                   level' <- getState
                                   return $ NumberedList level' (trim cs'')
              isListLine :: String -> Bool
              isListLine cs = case parse p "" cs of
                                    Right _ -> True
                                    Left _ -> False
                                  where
                                    p :: Parser Bool
                                    p = do skipMany (space <|> tab)
                                           listmark <|> numberedlistmark
                                           return True
              listmark = satisfy (`elem` "*+-")
              numberedlistmark = digit >> (char '.')

paragraph :: LineParser [Node]
paragraph = do manyLines <- many1 $ lineSatisfy isNotMarkAndBlankLine
               return $ map Paragraph manyLines
            where
              isNotMarkAndBlankLine :: String -> Bool
              isNotMarkAndBlankLine cs = (((0 < ) . length . trim) cs) && (isNotMarkLine cs)

isNotMarkLine :: String -> Bool
isNotMarkLine cs = case parse markParser "" cs of
                      Right _ -> False
                      Left _ -> True

markParser :: Parser Bool
markParser = do isMark
                return False
             <|> do digit
                    satisfy (== '.')
                    return False
             where
               isMark = satisfy (`elem` "#*+->")

document :: LineParser String
document = do nodes <- many1 block
              eof
              return $ join $ intersperse "\n" $ map compile (concat nodes)

block :: LineParser [Node]
block = do blank
           return $ [Paragraph ""]
        <|> headline
        <|> listblock
        <|> paragraph

main :: IO()
main = do cs <- getContents
          case parse document "" (lines cs) of
            Right str -> putStr str
            Left err -> putStr $ show err
