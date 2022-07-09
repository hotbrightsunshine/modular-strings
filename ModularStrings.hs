-- Modular Strings, Riccardo Pratesi
-- Script edited by Francesco Parisio

module ModularStrings where

zeros :: Int -> [Int]
zeros n = replicate n 0

primordial n = zeros (n-1) ++ [1]

type Modular = [Int]
type Evolutions = [Modular]

compute :: Int -> Int -> Int -> Int
compute a b n = (a+b) `mod` n

evolve :: Modular -> Int -> Int -> Modular
evolve (x:xs) modulo first =
  if null xs then
    [compute x first modulo]
  else
    compute x (head xs) modulo : evolve xs modulo first
evolve [] _ _ = [] 

-- Returns an Infinite list of evolutions!
evolution :: Modular -> Evolutions
evolution m = m : evolution ( evolve m (length m) (head m) )

evolutionsOf :: Int -> Int -> Evolutions
evolutionsOf k m = take k $ evolution $ primordial m
