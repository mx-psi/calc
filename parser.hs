import Data.List
import Data.Maybe
import Data.Ord
import Data.List.Split (splitOn)

data Expression = Var String | Val Double | Op String [Expression] deriving Eq
instance Show Expression where
  show (Var x)      = x
  show (Val x)      = show x
  show (Op op list) = "(" ++ intercalate (" "++op++" ") (map show list) ++ ")"


---- Functions for replacing ----

replace :: Eq a => [a] -> [a] -> [a] -> [a]
-- Replaces sublist in list
replace [] _ ys = ys
replace  _ _ [] = []
replace sub rep ys@(x:xs)
  |not (sub `isInfixOf` ys) = ys
  |sub `isPrefixOf` ys      = rep ++ replace sub rep (drop (length sub) ys)
  |otherwise                = x : replace sub rep xs

substitute :: Expression -> Expression -> Expression -> Expression
-- Replaces Expressions
substitute sub rep (Op op ys)
  | sub /= Op op ys = Op op (map (substitute sub rep) ys)
substitute sub rep val  = if sub == val then rep else val

applyAssoc  :: (a -> a -> a -> a) -> [(a, a)] -> a -> a
-- Compose a function applied to elements of a list
applyAssoc f = foldl (.) id . map (uncurry f)

replaceList :: (Eq a) => [([a],[a])] -> [a] -> [a]
-- Replaces in given order each pair of sublists in list
replaceList = applyAssoc replace

substituteList :: [(Expression, Expression)] -> Expression -> Expression
-- Replaces list of expressions in order.
substituteList = applyAssoc substitute


---- Brackets handling ----

add n (f,s) = (f + n, s + n)

valChar :: Char ->Int
-- Assigns value to each char
valChar '(' =  1
valChar ')' = -1
valChar  _  =  0

paren :: Int -> String -> Int
-- Returns index of closing bracket.
paren 0    _   = 0
paren _    []  = error "paren: Mismatch"
paren n (x:xs) = 1 + paren (n + valChar x) xs

outBrackets :: String -> Maybe (Int,Int)
-- Returns indices of the first outermost brackets
outBrackets ys
  | '(' `notElem` ys  = Nothing
outBrackets ('(':xs)  = Just (0, paren 1 xs)
outBrackets ( _:xs )  = fmap (add 1) (outBrackets xs)

anyBrackets :: String -> Bool
-- Checks if there are any (matching) brackets
anyBrackets = isJust . outBrackets

coordsList :: String -> [(Int,Int)]
-- Returns list of outermost brackets
coordsList ys
  | not (anyBrackets ys) = []
  | otherwise      = (s,e) : map (add (e+1)) (coordsList (drop (e+1) ys))
    where Just (s,e) = outBrackets ys

removeBrackets :: String -> String
-- Removes brackets. Unsafe
removeBrackets = tail . init

slice :: [a] -> (Int, Int) -> [a]
-- Gets slice of list. Uncurry to simplify calls
slice ys (from,to) = take (to - from + 1) (drop from ys)

bracketsList :: String -> [String]
-- Gets brackets content list sorted by length
bracketsList ys = sortBy (comparing (Down . length)) (map (slice ys) (coordsList ys))


---- Expression data type related functions ----

isNum :: String -> Bool
-- Tells if string is a readable Double
isNum = all (`elem` ".-0123456789")

-- Auxiliary variables and expressions
aux = map (\c->'_':[c]) ['A'..]
expAux = map Var aux

parse :: [String] -> String -> Expression
-- Parses string given list of binary operations

-- Base cases
parse _ "" = Val 0
parse [] ys
  | isNum ys  = Val (read ys :: Double)
  | otherwise = Var ys

-- Brackets parsing
parse opl ys
  | anyBrackets ys   =
    let bs     = bracketsList ys -- String content of brackets
        expBs  = map (parse opl . removeBrackets) bs -- Parsed contents
    in substituteList (zip expAux expBs) (parse opl (replaceList (zip bs aux) ys))

-- Ops parsing
parse (op:ops) ys
  | op `isInfixOf` ys = Op op (map (parse ops) (replace [""] [] (splitOn op ys)))
  | otherwise         = parse ops ys

---- Operations and related functions ----

operators :: [(String, Double -> Double -> Double)]
-- Symbols and related functions, ordered by precedence
operators = [("+",(+)), ("*",(*)), ("/",(/)), ("^",(**)), ("-", (-))]

getOp :: String -> (Double -> Double -> Double)
-- Get associated function. Assumes no failure.
getOp op = fromJust (lookup op operators)

value :: Expression -> Maybe Double
-- Gets numerical value of an expression
value (Var _)    = Nothing
value (Val x)    = Just x
value (Op op ys) = fmap (foldl1' (getOp op)) (mapM value ys)

readExp = parse (map fst operators) . filter (/=' ')
calc = value . readExp
main = getLine >>= print . calc
