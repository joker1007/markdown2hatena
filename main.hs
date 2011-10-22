import System
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec
import LineParser
import Trim

data Node = HeadLine Int String
            | ListLine Int String
            | Paragraph String
            deriving (Show)

compile :: Node -> String
compile (HeadLine level text) = (join $ take level $ repeat "*") ++ text
compile (ListLine level text) = (join $ take (level-1) $ repeat "  ") ++ "- " ++ text
compile (Paragraph "") = "\n"
compile (Paragraph text) = text

sample :: String
sample = "### 見出し1\n#見出し\n\ntest\ntest\n\n*list\n*list2"

headline :: LineParser Node
headline = do line <- firstChar (== '#')
              let (mark, str) = span (== '#') line
              return $ HeadLine (length mark) (trim str)

listline :: LineParser Node
listline = do line <- firstChar (isListMark)
              let (mark, str) = span (isListMark) line
              return $ ListLine 1 (trim str)
           where
              isListMark :: Char -> Bool
              isListMark '*' = True
              isListMark '+' = True
              isListMark '-' = True
              isListMark _ = False

paragraph :: LineParser Node
paragraph = do lines <- notBlank
               return $ Paragraph lines

run :: Show a => Parser a -> String -> IO()
run p input = case (parse p "" input) of
                Left err -> do putStr "parse error at "
                               print err
                Right x ->  do print x

document :: LineParser String
document = do nodes <- many1 block
              eof
              return $ join $ intersperse "\n" $ map compile nodes

block :: LineParser Node
block = do blank
           return $ Paragraph ""
        <|> headline
        <|> listline
        <|> paragraph

main :: IO()
main = do cs <- getContents
          case parse document "" (lines cs) of
            Right str -> putStr str
            Left err -> putStr $ show err
