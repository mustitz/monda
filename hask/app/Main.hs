module Main where

import Text.Parsec
import Text.Parsec.String

number :: Parser Int
number = read <$> many1 digit

numbers :: Parser [Int]
numbers = number `sepBy` char ','

main :: IO ()
main = do
  let input = "1,2,3,42"
  case parse numbers "" input of
    Left err -> print err
    Right nums -> print nums
