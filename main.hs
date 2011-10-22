import Data.Char
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec

data Node = HeadLine Int String
            | ListLine Int String
            | Text String
            deriving (Show)

compile :: Node -> String
compile (HeadLine level text) = (join $ take level $ repeat "*") ++ text
compile (ListLine level text) = (join $ take (level-1) $ repeat "  ") ++ "- " ++ text
compile (Text text) = text

text :: String
text = "### 見出し1  \n#見出し2"

node :: Node
node = HeadLine 2 "タイトル"

node2 :: Node
node2 = ListLine 2 "リスト"

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

firstChar :: (Char -> Bool) -> LineParser String
firstChar f = lineSatisfy (test f)
  where
    test f "" = False
    test f (c:_) = f c

headline :: LineParser Node
headline = do line <- firstChar (== '#')
              let (mark, text) = span (== '#') line
              let level = length mark
              return $ HeadLine (length mark) (trim text)


ltrim :: String -> String
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

simple :: Parser Char
simple = digit

block :: LineParser [Node]
block = do hs <- many headline
           return hs

main :: IO()
main = do case parse block "" (lines text) of
            Right nodes -> putStr $ concat $ intersperse "\n" $ map compile nodes
            Left err -> putStr $ show err
