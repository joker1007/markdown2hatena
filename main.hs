import Data.Char
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec
import LineParser

data Node = HeadLine Int String
            | ListLine Int String
            | Text String
            deriving (Show)

compile :: Node -> String
compile (HeadLine level text) = (join $ take level $ repeat "*") ++ text
compile (ListLine level text) = (join $ take (level-1) $ repeat "  ") ++ "- " ++ text
compile (Text text) = text

sample :: String
sample = "### 見出し1  \n#見出し2"

headline :: LineParser Node
headline = do line <- firstChar (== '#')
              let (mark, str) = span (== '#') line
              return $ HeadLine (length mark) (trim str)


ltrim :: String -> String
ltrim "" = ""
ltrim (x:xs) | isSpace x = ltrim xs
             | otherwise = (x:xs)

rtrim :: String -> String
rtrim = reverse . ltrim . reverse

trim :: String -> String
trim = ltrim . rtrim

run :: Show a => Parser a -> String -> IO()
run p input = case (parse p "" input) of
                Left err -> do putStr "parse error at "
                               print err
                Right x ->  do print x

block :: LineParser [Node]
block = do hs <- many headline
           return hs

main :: IO()
main = do case parse block "" (lines sample) of
            Right nodes -> putStr $ concat $ intersperse "\n" $ map compile nodes
            Left err -> putStr $ show err
