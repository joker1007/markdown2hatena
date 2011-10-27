module Node
  (Node(..), compile) where

  import Text.ParserCombinators.Parsec
  import Control.Monad
  data Node = HeadLine Int String
              | ListLine Int String
              | NumberedList Int String
              | QuotoBlock String
              | Paragraph String
              deriving (Show)

  compile :: Node -> String
  compile (HeadLine level text) = (join $ take level $ repeat "*") ++ (inlineParse text)
  compile (ListLine level text) = (join $ take level $ repeat "-") ++ (inlineParse text)
  compile (NumberedList level text) = (join $ take level $ repeat "+") ++ (inlineParse text)
  compile (QuotoBlock text) = ">> " ++ text ++ " <<"
  compile (Paragraph "") = "\n"
  compile (Paragraph text) = (inlineParse text)

  inlineParse :: String -> String
  inlineParse cs = case runParser p "" "" cs of
                     Right cs' -> cs'
                     Left err -> show err
                   where
                     p :: GenParser Char String String
                     p = do elem <- try(link)
                            updateState (++ elem)
                            p
                         <|> do c <- anyChar
                                updateState (++ [c])
                                p
                         <|> do eof
                                line <- getState
                                return line

  link :: GenParser Char String String
  link = do char '['
            cs <- many1 $ noneOf "]"
            char ']'
            char '('
            x <- (string "http://") <|> (string "https://")
            x' <- many1 $ noneOf " )"
            char ')'
            return ("[" ++ x ++ x' ++ ":title=" ++ cs ++ "]")

