import Data.List
import Control.Monad
import Data.Maybe
import Data.Ord
import Data.List.Split (splitOn)

data Expression = Var String | Val Double | Op String [Expression] deriving Eq
instance Show Expression where
  show (Var x)      = x
  show (Val x)      = show x
  show (Op op list) = "(" ++ intercalate (" "++op++" ") (map show list) ++ ")"

---- Basic functions ----

slice :: (Int, Int) -> [a] -> [a]
-- Returns slice of lists between indices.
-- Uncurry version to simplify calls
slice (from,to) | to < from = \_ -> []
slice (from,to) = take (to - from + 1 :: Int) . drop from

-- Removes brackets
removeBrackets ys = slice (1,(length ys)-2) ys

-- Adds n to pair
add n (f,s) = (f + n, s + n)

---- Functions for replacing ----

replace :: Eq a => [a] -> [a] -> [a] -> [a]
-- Replaces sublist in list
replace [] _ ys = ys
replace _ _ [] = []
replace sub rep ys@(x:xs)
  |not (sub `isInfixOf` ys) = ys
  |sub `isPrefixOf` ys      = rep ++ replace sub rep (drop (length sub) ys)
  |otherwise                = x:(replace sub rep xs)

substitute :: Expression -> Expression -> Expression -> Expression
-- Replaces Expressions
substitute sub rep (Op op ys)
  | sub /= (Op op ys) = Op op (map (substitute sub rep) ys)
substitute sub rep val  = if (sub == val) then rep else val

applyAssoc  :: (a -> a -> a -> a) -> [(a, a)] -> a -> a
-- Compose a function applied to elements of a list
applyAssoc f = foldl (.) id . map (\(s,r) -> f s r)

replaceList :: (Eq a) => [([a],[a])] -> [a] -> [a]
-- Replaces in given order each pair of sublists in list
replaceList = applyAssoc replace

substituteList :: [(Expression, Expression)] -> Expression -> Expression
-- Replaces list of expressions in order.
substituteList = applyAssoc substitute


---- Brackets handling ----

paren :: Int -> String -> Int
paren 0 _  = 0
paren _ [] = error "paren: brackets mismatching"
paren n (x:xs)
  | x == '('  = 1 + paren (n+1) xs
  | x == ')'  = 1 + paren (n-1) xs
  | otherwise = 1 + paren n xs

outBrackets :: String -> Maybe (Int,Int)
-- Returns indices of the first outermost brackets
outBrackets ""            = Nothing
outBrackets ys@(x:xs)
  | not ('(' `elem` ys) = Nothing
  | x == '('            = Just (0, paren 1 xs)
  | otherwise           = Just (add 1(a, b))
      where Just (a,b)  = outBrackets xs

anyBrackets :: String -> Bool
-- Checks if there are any (matching) brackets
anyBrackets = isJust . outBrackets

coordsList :: String -> [(Int,Int)]
-- Returns list of outermost brackets
coordsList ys
  | not (anyBrackets ys) = []
  | otherwise      = (s,e):(map (add (e+1)) (coordsList (drop (e+1) ys)))
    where Just (s,e) = outBrackets ys

bracketsList ys = sortBy (comparing (Down . length)) (map (flip slice ys) (coordsList ys))

aux = map (\c->'_':[c]) ['A'..] -- Auxiliary variables to replace brackets

---- Expression data type related functions ----

isNum :: String -> Bool
isNum = all (`elem` ".-0123456789")

fromString :: [String] -> String -> Expression
-- Converts string to expression given list of binary operations
fromString _ "" = Val 0
fromString opl@(op:ops) ys
  | anyBrackets ys   =
    let bs     = bracketsList ys
        expBs  = map (fromString opl . removeBrackets) bs
        expAux = map Var aux
    in substituteList (zip expAux expBs) (fromString opl (replaceList (zip bs aux) ys))
  | op `isInfixOf` ys = Op op (map (fromString ops) (replace [""] [] (splitOn op ys)))
  | otherwise         = fromString ops ys

fromString [] ys
  | isNum ys  = Val (read ys :: Double)
  | otherwise = Var ys

---- Operations and related functions ----

-- Symbols and related functions, ordered by precedence
ops = [("+",(+)), ("*",(*)), ("/",(/)), ("^",(**))]
-- Get associated function. Assumes no failure.
getOp op = fromJust (lookup op ops)

readExp = fromString (map fst ops) . replaceList [(" ",""),("-","+-")]

value :: Expression -> Maybe Double
value (Var _)    = Nothing
value (Val x)    = Just x
value (Op op ys) = liftM (foldr1 (getOp op)) (mapM value ys)
