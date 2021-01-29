import Data.Char

-- Parser Type

type Parse a b = [a] -> [(b,[a])]


-- Basic Parsers

none :: Parse a b
none inp = []

succeed :: b -> Parse a b
succeed val inp = [(val,inp)]


-- Basic Parser Builders

token :: Char -> Parse Char Char
token t [] = []
token t (x:xs) 
 | t == x    = [(t,xs)]
 | otherwise = []

spot :: (a->Bool) -> Parse a a
spot p [] = []
spot p (x:xs)
 | p x       = [(x,xs)]
 | otherwise = []


-- Parser Combinators

alt :: Parse a b -> Parse a b -> Parse a b
alt p1 p2 inp = (p1 inp) ++ (p2 inp)

sqn :: Parse a b -> Parse a c -> Parse a (b,c)
sqn p1 p2 inp
 = [((v1,v2),rem2) | (v1,rem1) <- p1 inp,
                     (v2,rem2) <- p2 rem1 ]

build :: Parse a b -> (b->c) -> Parse a c
build p f inp = [ (f x, rem) | (x,rem) <- p inp ]

list :: Parse a b -> Parse a [b]
list p = alt (succeed [])
             (build (sqn p (list p))
                    (uncurry (:)))



-- Expressions

data Expr = Lit Int |
            Var Char |
            Op Ops Expr Expr deriving Show
data Ops = Add | Sub | Mul | Div | Mod deriving Show

showExpr :: Expr -> String
showExpr (Lit n)       
 | n < 0               = "~" ++ (show (-n))
 | otherwise           = show n
showExpr (Var ch)      = [ch]
showExpr (Op op e1 e2) = "(" ++ (showExpr e1) ++ (showOps op)
                             ++ (showExpr e2) ++ ")"

showOps :: Ops -> String
showOps Add = "+"
showOps Sub = "-"
showOps Mul = "*"
showOps Div = "/"
showOps Mod = "%"


-- Expression Parser (Top-Level)

parser :: Parse Char Expr
parser = alt varParse
             (alt opExpParse
                  litParse)

showParse :: [(Expr,[Char])] -> String
showParse [] = ""
showParse [x] = showParse1 x
showParse (x:xs) = (showParse1 x) ++ ", " ++ (showParse xs)

showParse1 :: (Expr,[Char]) -> String
showParse1 (e,rem) = "(" ++ (showExpr e) ++ ", \"" ++ rem ++ "\")"


-- Variable Parser

varParse :: Parse Char Expr
varParse = build (spot isVar) Var

isVar :: Char -> Bool
isVar x = ('a' <= x && x <= 'z')


-- Operator Expression Parser

opExpParse :: Parse Char Expr
opExpParse = build (sqn (token '(')
                        (sqn parser
                             (sqn (spot isOp)
                                  (sqn parser
                                       (token ')')))))
                   makeExpr


-- Literal Parser

litParse :: Parse Char Expr
litParse
  = build (sqn (optional (token '~'))
               (neList (spot isDigit)))
          (stringToExpr . uncurry (++))

-- Your code goes here ...
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
--tut parti question
nTimes :: Int -> Parse a b -> Parse a [b]
nTimes 0 p = succeed []
nTimes n p = build (sqn p (nTimes (n-1) p))  (uncurry (:))