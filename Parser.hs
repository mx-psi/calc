module Parser where

import Data.List (isInfixOf, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Tree

---- Functions for replacing ----

replace :: Eq a => [a] -> [a] -> [a] -> [a]
-- Replaces sublist in list
replace [] _ ys = ys
replace _ _ [] = []
replace sub rep ys@(x : xs)
  | not (sub `isInfixOf` ys) = ys
  | sub `isPrefixOf` ys = rep ++ replace sub rep (drop (length sub) ys)
  | otherwise = x : replace sub rep xs

substitute :: Eq a => Tree a -> Tree a -> Tree a -> Tree a
-- Replaces elements in Tree
substitute sub rep tree@(Node v xs) =
  if sub == tree then rep else Node v (map (substitute sub rep) xs)

compose :: (a -> a -> b -> b) -> [(a, a)] -> b -> b
-- Compose a function applied to elements of a list
compose f = foldl (.) id . map (uncurry f)

replaceList :: Eq a => [([a], [a])] -> [a] -> [a]
-- Replaces in given order each pair of sublists in list
replaceList = compose replace

substituteList :: Eq a => [(Tree a, Tree a)] -> Tree a -> Tree a
-- Replaces list of Trees in order.
substituteList = compose substitute

---- Brackets ----

add n (f, s) = (f + n, s + n)

paren :: Int -> String -> Int
-- Index of closing bracket
paren 0  _ = 0
paren _ [] = error "paren: Mismatch"
paren n (x : xs) = 1 + paren (n + valChar x) xs
  where valChar '(' =  1
        valChar ')' = -1
        valChar  _  =  0

outBrackets :: String -> Maybe (Int, Int)
-- Indices of the first outermost brackets
outBrackets ys | '(' `notElem` ys = Nothing
outBrackets ('(' : xs) = Just (0, paren 1 xs)
outBrackets ( _  : xs) = fmap (add 1) (outBrackets xs)

coordsList :: String -> [(Int, Int)]
-- List of outermost brackets
coordsList ys
  | '(' `notElem` ys = []
  | otherwise = (s, e) : map (add (e + 1)) (coordsList (drop (e + 1) ys))
    where  Just (s, e) = outBrackets ys

slice :: [a] -> (Int, Int) -> [a]
-- Slice of list. Uncurry to simplify calls
slice ys (from, to) = take (to - from + 1) (drop from ys)

---- Parsing ----

-- Auxiliary variables and Trees
aux    = map (\ c -> '_' : c : "_") ['A' ..]
expAux = map return aux

parse :: [String] -> String -> Tree String
-- Parses string given list of binary operations
parse _ "" = error "parse: Empty string."

-- Auxiliary variables
parse [] ys@('_' : c : '_' : xs)
  | null xs   = return ys
  | otherwise = error "parse: Bad brackets or underscores."

-- Function calls
parse [] ys
  | '_' `elem` ys = let (op, val) = span (/= '_') ys in Node op [parse [] val]
  | otherwise = return ys

-- Brackets
parse opl ys | '(' `elem` ys =
  let bs    = map (slice ys) (coordsList ys)   -- String content of brackets
      expBs = map (parse opl . tail . init) bs -- Parsed contents
  in substituteList (zip expAux expBs) $ parse opl $ replaceList (zip bs aux) ys

-- Operations
parse (op : ops) ys
  | op `isInfixOf` ys = Node op (map (parse ops) (splitOn op ys))
  | otherwise         = parse ops ys
