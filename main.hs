import System
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import LineParser
import Trim
import Node

sample :: String
sample = "### 見出し1\n#見出し\n\ntest\ntest\n\n*list\n*list2"

headline :: LineParser [Node]
headline = do line <- firstChar (== '#')
              let (mark, str) = span (== '#') line
              return $ [HeadLine (length mark) (trim str)]

listblock :: LineParser [Node]
listblock = do lines <- many1 (firstChar (isListMark))
               sublines <- many (lineSatisfy isIndentedList)
               let str = map (snd . span (isListMark)) lines
                   nodes = map (ListLine 1 . trim) str
                   subnodes = map (subListParse) sublines
               return (nodes ++ subnodes)
            where
              isListMark :: Char -> Bool
              isListMark = (`elem` "*+-")
              subListParse :: String -> Node
              subListParse cs = case runParser levelParser 1 "" cs of
                                  Right (level, str'') -> ListLine level str''
                                  Left err -> Paragraph $ show err
              levelParser :: GenParser Char Int (Int, String)
              levelParser = do try (count 4 space)
                               updateState (+1)
                               levelParser
                            <|> do tab
                                   updateState (+1)
                                   levelParser
                            <|> do satisfy isListMark
                                   cs' <- many anyChar
                                   level' <- getState
                                   return (level', trim cs')

isIndentedList :: String -> Bool
isIndentedList cs = case parse p "" cs of
                      Right _ -> True
                      Left _ -> False
                    where
                      p :: Parser Bool
                      p = do skipMany1 (space <|> tab)
                             satisfy (`elem` "*+-")
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

run :: Show a => Parser a -> String -> IO()
run p input = case (parse p "" input) of
                Left err -> do putStr "parse error at "
                               print err
                Right x ->  do print x

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
