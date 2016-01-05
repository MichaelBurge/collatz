module Collatz where

numToCollatz :: Integer -> [Integer]
numToCollatz x =
  let helper :: Integer -> Integer -> [Integer]
      helper numOnes n =
        case n of
          0 -> if numOnes == 0
               then []
               else [numOnes]
          _ | odd n -> helper (numOnes + 1) (n `div` 2)
          _ | even n -> numOnes:(numToCollatz $ n `div` 2)
          _ -> error "numToCollatz: ???"
  in helper 0 x

collatzToNum :: [Integer] -> Integer
collatzToNum [] = 0
collatzToNum (x:xs) = (2^x-1) + (2^(x+1)) * (collatzToNum xs)
