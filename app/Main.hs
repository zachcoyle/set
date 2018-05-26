module Main where

import Data.List
import System.Random
import System.Random.Shuffle (shuffle')

data Color
  = Red
  | Green
  | Purple
  deriving (Eq, Ord, Show, Read, Enum)

data Symbol
  = Squiggle
  | Diamond
  | Oval
  deriving (Eq, Ord, Show, Read, Enum)

data Shading
  = Solid
  | Stripes
  | Outline
  deriving (Eq, Ord, Show, Read, Enum)

data Card = Card
  { color :: Color
  , symbol :: Symbol
  , shading :: Shading
  , number :: Int
  } deriving (Eq, Ord, Show, Read)

deck :: [Card]
deck = do
  color <- [Red ..]
  symbol <- [Squiggle ..]
  shading <- [Solid ..]
  number <- [1, 2, 3]
  return
    Card {color = color, symbol = symbol, shading = shading, number = number}

allEq :: (Eq a) => [a] -> Bool
allEq xs = length (nub xs) == 1

noneEq :: (Eq a) => [a] -> Bool
noneEq xs = length (nub xs) == 3

isMatch' :: (Eq a) => [a] -> Bool
isMatch' xs = allEq xs || noneEq xs

dedupe = nub . fmap sort

isMatch xs =
  all
    (== True)
    [isMatch' colors, isMatch' numbers, isMatch' symbols, isMatch' shadings]
  where
    colors = fmap color xs
    numbers = fmap number xs
    symbols = fmap symbol xs
    shadings = fmap shading xs

findMatches :: [Card] -> [[Card]]
findMatches xs = dedupe $ filter isMatch triples
  where
    triples = [[x, y, z] | x <- xs, y <- xs, z <- xs, noneEq [x, y, z]]

main :: IO ()
main = do
  gen <- newStdGen
  let shuffledDeck = shuffle' deck (length deck) gen
  let board = take 9 shuffledDeck
  putStrLn "board :"
  print $ length board
  mapM_ print board
  putStrLn "--------------------"
  putStrLn "results:"
  let matches = findMatches board
  print $ show (length matches) ++ " sets"
  mapM_ print matches
