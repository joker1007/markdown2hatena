import System
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import LineParser
import Trim
import Node

headline :: LineParser [Node]
headline = do line <- firstChar (== '#')
              let (mark, str) = span (== '#') line
              return $ [HeadLine (length mark) (trim str)]

listblock :: LineParser [Node]
listblock = do lines <- many1 (lineSatisfy isListLine)
               return $ map listParse lines
            where
              listParse :: String -> Node
              listParse cs = case runParser levelParser 1 "" cs of
                                  Right (level, cs') -> ListLine level cs'
                                  Left err -> Paragraph $ show err
              levelParser :: GenParser Char Int (Int, String)
              levelParser = do try (count 4 space)
                               updateState (+1)
                               levelParser
                            <|> do tab
                                   updateState (+1)
                                   levelParser
                            <|> do satisfy (`elem` "*+-")
                                   cs'' <- many anyChar
                                   level' <- getState
                                   return (level', trim cs'')

isListLine :: String -> Bool
isListLine cs = case parse p "" cs of
                      Right _ -> True
                      Left _ -> False
                    where
                      p :: Parser Bool
                      p = do skipMany (space <|> tab)
                             satisfy (`elem` "*+-")
                             return True


numberedlistblock :: LineParser [Node]
numberedlistblock = do lines <- many1 (lineSatisfy isNumberedListLine)
                       return $ map listParse lines
                    where
                      listParse :: String -> Node
                      listParse cs = case runParser levelParser 1 "" cs of
                                          Right (level, cs') -> NumberedList level cs'
                                          Left err -> Paragraph $ show err
                      levelParser :: GenParser Char Int (Int, String)
                      levelParser = do try (count 4 space)
                                       updateState (+1)
                                       levelParser
                                    <|> do tab
                                           updateState (+1)
                                           levelParser
                                    <|> do digit
                                           char '.'
                                           cs'' <- many anyChar
                                           level' <- getState
                                           return (level', trim cs'')

isNumberedListLine :: String -> Bool
isNumberedListLine cs = case parse p "" cs of
                      Right _ -> True
                      Left _ -> False
                    where
                      p :: Parser Bool
                      p = do skipMany (space <|> tab)
                             digit
                             char '.'
                             return True

paragraph :: LineParser [Node]
paragraph = do lines <- many1 $ lineSatisfy isNotMarkAndBlankLine
               return $ map Paragraph lines
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
        <|> numberedlistblock
        <|> paragraph

main :: IO()
main = do cs <- getContents
          case parse document "" (lines cs) of
            Right str -> putStr str
            Left err -> putStr $ show err
