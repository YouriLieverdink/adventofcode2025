module Main where

sheep :: [Char] -> Int
sheep ('L' : xs) = -(read xs :: Int)
sheep ('R' : xs) = read xs :: Int
sheep xs = read xs :: Int

cow :: [Int] -> [Int]
cow = scanl (\a b -> (a + b) `mod` 100) 50

chicken :: [Int] -> Int
chicken [] = 0
chicken (x : xs)
  | x == 0 = 1 + chicken xs
  | otherwise = chicken xs

main :: IO ()
main = do
  content <- readFile "assets/example.txt"
  print $ cow $ map sheep (lines content)
