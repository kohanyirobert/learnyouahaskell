solveRPN :: String -> Float
solveRPN = head . foldl reduceRPN [] . words
  where
    reduceRPN (x:y:ys) "+" = y + x : ys
    reduceRPN (x:y:ys) "-" = y - x : ys
    reduceRPN (x:y:ys) "*" = y * x : ys
    reduceRPN (x:y:ys) "^" = y ** x : ys
    reduceRPN (x:ys) "ln" = log x : ys
    reduceRPN xs "sum" = [sum xs]
    reduceRPN xs x = read x : xs
