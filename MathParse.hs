{- Example of Math parser -}
import Parser
import Data.Maybe (fromJust, isJust)
import Data.Tree
import Data.Tree.Pretty

operators = [(",", const), ("-", (-)), ("+", (+)), ("/", (/)), ("*", (*)), ("^", (**))]

unaryFun = [("sqrt", sqrt), ("abs", abs), ("inv", recip), ("log", log),
            ("sin", sin), ("cos", cos), ("tan", tan)]

isNum :: String -> Bool
-- Tells if string is a readable Double
isNum = all (`elem` ".-0123456789")

{- Calculations -}

value :: Tree String -> Either String Double
-- Gets numerical value of an expression
value (Node a  [])
  | isNum a   = Right $ read a
  | otherwise = Left  $ "Unknown expression: " ++ a

value (Node f [x])
  | isJust f' = fmap (fromJust f') (value x)
  | otherwise = Left $ "Unknown function: " ++ f
    where f'  = lookup f unaryFun

value (Node op ys)
  | isJust op' = fmap (foldl1 (fromJust op')) (mapM value ys)
  | otherwise  = Left $ "Unknown operator: " ++ op
    where op'  = lookup op operators

readExp = parse (map fst operators) . filter (/= ' ')
calc = value . readExp
main = do
        expr <- getLine
        putStrLn $ drawVerticalTree $ readExp expr
        print $ calc expr
