module Node
  (module Control.Monad, Node(..), compile) where

  import Control.Monad
  data Node = HeadLine Int String
              | ListLine Int String
              | Paragraph String
              deriving (Show)

  compile :: Node -> String
  compile (HeadLine level text) = (join $ take level $ repeat "*") ++ text
  compile (ListLine level text) = (join $ take level $ repeat "-") ++ text
  compile (Paragraph "") = "\n"
  compile (Paragraph text) = text

  
