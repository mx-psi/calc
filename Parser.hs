module Parser where

import Data.List
import Data.Tree
import Data.Ord
import Data.List.Split (splitOn)

-- -- Functions for replacing ----

replace :: Eq a => [a] -> [a] -> [a] -> [a]
-- Replaces sublist in list
replace [] _ ys = ys
replace _ _ [] = []
replace sub rep ys@(x : xs)
  | not (sub `isInfixOf` ys) = ys
  | sub `isPrefixOf` ys = rep ++ replace sub rep (drop (length sub) ys)
  | otherwise = x : replace sub rep xs

substitute :: Eq a => Tree a -> Tree a -> Tree a -> Tree a
-- Replaces Expressions in Tree
substitute sub rep tree@(Node v xs) =
  if sub == tree then rep else Node v (map (substitute sub rep) xs)

applyAssoc :: (a -> a -> b -> b) -> [(a, a)] -> b -> b
-- Compose a function applied to elements of a list
applyAssoc f = foldl (.) id . map (uncurry f)

replaceList :: Eq a => [([a], [a])] -> [a] -> [a]
-- Replaces in given order each pair of sublists in list
replaceList = applyAssoc replace

substituteList :: Eq a => [(Tree a, Tree a)] -> Tree a -> Tree a
-- Replaces list of Expressions in order.
substituteList = applyAssoc substitute


-- -- Brackets handling ----

add n (f, s) = (f + n, s + n)

valChar :: Char -> Int
-- Assigns value to each char
valChar '(' = 1
valChar ')' = -1
valChar _ = 0

paren :: Int -> String -> Int
-- Returns index of closing bracket.
paren 0 _ = 0
paren _ [] = error "paren: Mismatch"
paren n (x : xs) = 1 + paren (n + valChar x) xs

outBrackets :: String -> Maybe (Int, Int)
-- Returns indices of the first outermost brackets
outBrackets ys
  | '(' `notElem` ys = Nothing
outBrackets ('(' : xs) = Just (0, paren 1 xs)
outBrackets ( _ : xs) = fmap (add 1) (outBrackets xs)

coordsList :: String -> [(Int, Int)]
-- Returns list of outermost brackets
coordsList ys
  | '(' `notElem` ys = []
  | otherwise = (s, e) : map (add (e + 1)) (coordsList (drop (e + 1) ys))
    where Just (s, e) = outBrackets ys

slice :: [a] -> (Int, Int) -> [a]
-- Gets slice of list. Uncurry to simplify calls
slice ys (from, to) = take (to - from + 1) (drop from ys)

bracketsList :: String -> [String]
-- Gets brackets content list sorted by length
bracketsList ys = sortBy (comparing (Down . length)) (map (slice ys) (coordsList ys))


-- -- Expression data type related functions ----

-- Auxiliary variables and Expressions
aux    = map (\ c -> '_' : c : "_") ['A' ..]
expAux = map return aux

parse :: [String] -> String -> Tree String
-- Parses string given list of binary operations

parse _ "" = error "parse: Empty string."

-- Variables
parse [] ys@('_' : c : '_' : xs)
  | null xs = return ys
  | otherwise = error "parse: Bad brackets or underscores."

-- Function calls
parse [] ys
  | '_' `elem` ys = let (op, val) = span (/= '_') ys in Node op [parse [] val]
  | otherwise = return ys

-- Brackets parsing
parse opl ys
  | '(' `elem` ys =
    let bs = bracketsList ys -- String content of brackets
        expBs = map (parse opl . tail . init) bs -- Parsed contents
    in substituteList (zip expAux expBs) (parse opl (replaceList (zip bs aux) ys))

-- Ops parsing
parse (op : ops) ys
  | op `isInfixOf` ys = Node op (map (parse ops) (splitOn op ys))
  | otherwise = parse ops ys
