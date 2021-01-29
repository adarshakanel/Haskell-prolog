--Adarsha Kanel
--30049820
--Refrence used: Thompson S. - Haskell_ The craft of functional programming-AW (2011)

module A4 where
--Q1
data Poly = PConst Int | PVar | PAdd Poly Poly | PMul Poly Poly deriving Show

--compilePoly takes a poly and returns a output that is type (Int -> Int)
compilePoly:: Poly -> (Int -> Int)
-- if poly is PConst i, return i in form (Int -> Int)
compilePoly (PConst i) = (\k -> i)
-- if poly is PVar, return k in form (Int -> Int)
compilePoly (PVar) = (\k -> k)
-- if poly is PAdd x y, return the recursion of compilePoly on x and k added with the recursion of compilePoly on y and k, returned in form (Int -> Int)
compilePoly (PAdd x y) = (\k -> (compilePoly x k) + (compilePoly y k))
-- if poly is PMul x y, return the recursion of compilePoly on x and k multiplied with the recursion of compilePoly on y and k, returned in form (Int -> Int)
compilePoly (PMul x y) = (\k -> (compilePoly x k) * (compilePoly y k))

--Q4	
--runningSums taken in [Int] and returns [Int] such that any element i of [Int] is the sum of all the elements before it in the list
runningSums:: [Int] -> [Int]
runningSums i = [0] ++ (zipWith (+) i (runningSums i))