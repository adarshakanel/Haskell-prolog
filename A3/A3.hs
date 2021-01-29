--Adarsha Kanel
-- UCID = 30049820
--CPSC 449

module A3 where

--Question 1

--a
-- Haskell algebraic type  Formula
data Formula = Var String | Not Formula | And Formula Formula | Or Formula Formula deriving Show

--b
--a Haskell expression that constructs aFormularepresentation of the formula “~(~p^~q)
expression = Not ( And (Not (Var "p")) (Not (Var "q")))
--c
--showFormula returns a String representation of the Formula
showFormula :: Formula -> String
-- if the Formula is just Var a, return a
showFormula (Var a) =   a 
-- if the Formula has a Not in front, place a ~ in front and continue recursing on formula a
showFormula (Not (a) ) =  "(" ++"~"  ++ showFormula (a)  ++ ")"
-- if Formula a b have a And infront, then place a "^" between he recursion of a and a recursion of b
showFormula (And a b ) =  "(" ++  showFormula (a)  ++ " " ++ "^"  ++ " " ++ showFormula(b)  ++ ")"
-- if Formula a b have a Or infront, then place a "v" between he recursion of a and a recursion of b
showFormula (Or a b ) =  "("  ++ showFormula (a)++" " ++ "v" ++" " ++  showFormula(b)  ++ ")"


--d
-- 
--rewrite returns a wff f’ such that f’ is logically equivalent to f,and f’ is in NNF
rewrite :: Formula -> Formula
--Var a = Var a
rewrite (Var a) = Var a
--Not of Not formula a = recursion of a
rewrite (Not(Not(a))) = rewrite a
--Not of And of a b = Or of recursion of Not a and recursion of Not b
rewrite (Not( And a b)) = Or (rewrite (Not (a))) (rewrite(Not (b)))
--Not of OR of a b = And of recursion of Not a and recursion of Not b
rewrite (Not( Or a b)) = And (rewrite (Not (a))) (rewrite(Not (b)))
-- not of a = not of recursion of a
rewrite ( Not a) = Not (rewrite a)
-- and of a b = And of recursion of a and recursion of b
rewrite (And a b) = And (rewrite a) (rewrite b)
--Or of a b = Or of recursion of a and recursion of b
rewrite ( Or a b) = Or (rewrite a) (rewrite b)


--Question 3

--a
-- lastElm returns the last element of the list argu-men
lastElm :: [a] -> a
-- take the last element of [a] and remove every other elem until none are left, and return a
lastElm (x) = foldr1 (\_ k -> k) x

--b
--takes a list of pred-icates and an entity as arguments, then applies every predicate to the entity, and returns True if and only if every predicate in the list returnsTrue
unanimous :: [a->Bool] -> a -> Bool
unanimous f a 
--continue to map will make a new list that has the results of where a has been used an an argument for the predicates and then maps the list to see if all elements == True, if all elements are True, return True
    | foldr1 (\x acc -> ((x==True) && (acc==True))) (map (\k -> k a) f) == True = True
-- if all elements =/= True, then return false
    | otherwise = False

--c
--taes three arguments:  a predicate for type-avalues, a function that trans-forms a type-avalue to a type-bvalue, and a list of type-avalues then tests  every  element  of  the  list  argument  using  the  given  predicate
selectiveMap :: (a->Bool) -> (a->b) -> [a] -> [b]
-- map will apply the preicate (a-b) on the elements of [a] such that the remaining elements returned true when filtered with (a->Bool)
selectiveMap a d c = map (\k -> (d k)) (filter (\x -> ((a x) == True)) c)

--Question 4

--a 

--iter will take a integer n and apply the predicate f on x n times
iter:: Integer-> (a->a) -> a -> a
-- if n = 0, return the x and donot apply predicate on it
iter 0 _ x = x
-- recursively run the predicate f on x and do n-1 each recursion until n = 0
iter n f x = f ( iter (n-1) f x)

--b
--double will take an int and double it
double:: Integer -> Integer
double i = 2*i

--powerofTwo will take a integer i and will return the power of 2^i
powerofTwo :: Integer -> Integer
-- 2^0 = 1
powerofTwo 0 = 1
-- call iter and gives it the predicate double to run 2^2, n-1 times
powerofTwo i 
    | i >= 0 = iter (i-1) double 2 
    | otherwise = 0