{- Numerical expressions parser -}
import Parser
import Data.Maybe (fromJust, isJust)
import Data.Tree
import Data.Tree.Pretty

operators :: [(String, Double -> Double -> Double)]
operators =
  [(",", const), ("-", (-)), ("+", (+)), ("/", (/)), ("*", (*)), ("^", (**))]

functions :: [(String, Double -> Double)]
functions =
  [ ("sqrt", sqrt)
  , ("abs" , abs)
  , ("log" , log)
  , ("sin" , sin)
  , ("cos" , cos)
  , ("tan" , tan)
  ]

isNum :: String -> Bool
isNum = all (`elem` ".-0123456789")

{- Calculations -}

value :: Tree String -> Either String Double
-- Gets numerical value of an expression
value (Node a [])
  | isNum a   = Right $ read a
  | otherwise = Left $ "Unknown expression \"" ++ a ++ "\""

value (Node f [x])
  | isJust f' = fmap (fromJust f') (value x)
  | otherwise = Left $ "Unknown function \"" ++ f ++ "\""
  where f' = lookup f functions

value (Node op ys)
  | isJust op' = fmap (foldl1 (fromJust op')) (mapM value ys)
  | otherwise  = Left $ "Unknown operator \"" ++ op ++ "\""
  where op' = lookup op operators

readExp :: String -> Tree String
readExp = parse (map fst operators) . filter (/= ' ')

calc :: String -> Either String Double
calc = value . readExp

main :: IO ()
main = do
  expr <- getLine
  putStrLn $ drawVerticalTree $ readExp expr
  putStrLn $ either ("Can't evaluate: " ++) show $ calc expr
