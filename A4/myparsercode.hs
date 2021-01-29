--Adarsha Kanel
--30049820
--Refrence used: Thompson S. - Haskell_ The craft of functional programming-AW (2011)
--Q3

--isOp takes in a char and returns true if the chat is an Op else return false
isOp :: Char -> Bool
isOp '+' = True
isOp '*' = True
isOp '/' = True
isOp '-' = True
isOp '%' = True
-- char is not a part of isOp, return false regardless of what it is
isOp _ = False

--charToOp takes in a char and returns the Ops representation of that char, if one exists
charToOp :: Char -> Ops
charToOp '+' = Add
charToOp '*' = Mul
charToOp '-' = Sub
charToOp '/' = Div
charToOp '%' = Mod

--makeExpr converts a nested sequence of paris into an Expr
makeExpr :: (Char, (Expr, (Char, (Expr, Char)))) -> Expr
makeExpr (_, (a, (e, (b,_)))) = Op (charToOp e) a b

--neList takes in p and recognizes a non-empty list of the objects which are recognized by p
neList :: Parse a b -> Parse a [b]
neList p = build ( sqn p (list p)) (uncurry (:))

--optional takes in p and recognizes such an object optionally
optional :: Parse a b -> Parse a [b]
optional p = alt (succeed []) (build p (\k -> k:[]))

--stringToExpr takes in [Char] and returns a expression representation of the [Char]
stringToExpr :: [Char] -> Expr
stringToExpr (a:aa) 
    | a == '~' = Lit (-(read aa ::Int ) )
    | otherwise = Lit (read (a:aa)::Int)