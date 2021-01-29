--Adarsha Kanel
-- UCID = 30049820
--CPSC 449
--Refrence used : https://www.purplemath.com/modules/polymult2.htm

module A2 where

type Poly = [Integer]

--Question 1 a

--addpoly will take in 2 inputs poly (p:pp) and poly (p1:pp1) and return a poly that is the sum of both the lists
addpoly :: Poly -> Poly -> Poly
addpoly [] [] = []
addpoly i [] = i
addpoly [] i = i
addpoly (p:pp) (p1:pp1) 
--if pp and pp1 are both not empty, add the heads and do recursion of pp and pp1
    | (pp /= [] && pp1 /= []) = [(p + p1)] ++ addpoly pp pp1
-- pp is empty but pp1 is not, add the heads and concatinate the rest of pp1 and end recursion
    | (pp == [] && pp1 /= []) = [(p + p1)] ++ pp1
-- pp is not empty but pp1 is, add the heads of both lists and concatinate the rest of pp and end recursion
    | (pp /= [] && pp1 == []) = [(p + p1)] ++ pp
-- both pp and pp1 are empty, add the heads and end recursion
    | otherwise = [(p + p1)]
-------------------------------------------------------------------------------

--Question 1b
--multpoly will take in poly p and poly p1 and return a list that is the p * p1
multpoly :: Poly -> Poly -> Poly
-- if p1 is empty, return []
multpoly _ [] = []
-- if p is empty, return []
multpoly [] _ = []
multpoly (p) (p1) = addlist (multipoly_help p p1 (elemindex p 0))

--addlist will take [[Integer]] and add all the element of the same index in each list, and return a Poly consisting of it
addlist :: [[Integer]] -> Poly
addlist [] = []
addlist [x] = x
addlist (p:pp:ppp) =  addlist (ppp ++ [(addpoly p pp)])

--multpoly help will take a poly (p:pp) a poly (p:pp) and [Int] (o:oo) and return [[Integer]] that is filled with list that contains multiple lists that are [zero depending on where the element was in (p:pp) ++  p * (p:pp)]
multipoly_help :: Poly -> Poly -> [Int] -> [[Integer]]
multipoly_help [] _ _= []
-- will add n 0's in each list depending on the index n of the element and then concatinate each list with the values p * (p1:pp1)
multipoly_help (p:pp) (p1:pp1) (o:oo) = [(replicate ( o) 0)++ listmake (p:pp) (p1:pp1)] ++ multipoly_help pp (p1:pp1) oo

--elemindex will take a poly and a int and return a list that contains integers that correspond to the index they are present in the list( ie [0,1,2,3,4] )
elemindex :: Poly -> Int -> [Int]
elemindex [] _ = []
-- each element in poly represents the index number 
elemindex (p:pp) i= [i] ++ elemindex pp (i+1)

--listmake will take poly (p:pp) and poly(p1:pp1) and make a list of the values that occur when p is multiplied with (p1:pp1)
listmake :: Poly -> Poly -> Poly
listmake _ [] = []
listmake (p:pp) (p1:pp1) = [p * p1] ++ listmake (p:pp) (pp1)

-------------------------------------------------------------------------------
--Question 2a

--mergeLists will take 2 lists p and p1 and merge them into one list that is sorted in ascending order
mergeLists :: [Integer] -> [Integer] -> [Integer]
mergeLists [] [] = []
mergeLists [a] [] = [a]
mergeLists [] [b] = [b]
--calls ascending sort to sort (p++p1) in ascending order
mergeLists (p) (p1) = (ascendingsort (p ++ p1) (p++p1))

--ascendingsort will sort a poly p and poly pp and return p such that all its elements are sorted in ascending order
ascendingsort :: Poly -> Poly -> Poly
ascendingsort [] _ = []
-- will return the list p sorted in ascending order
ascendingsort (p) (pp)= multiplelow (lowestele (p)) pp  ++ ascendingsort (removeele (p) (lowestele p)) pp

--multiplelow will take an Integer i and Poly (p:pp) and return the a list filled with ALL the Integer i present is in (p:pp) 
multiplelow :: Integer -> Poly -> Poly
multiplelow _ [] = []
multiplelow i (p:pp)
-- will return all the Integer i present in (p:pp) in order to not lose the duplicates
    | i == p = [i] ++ multiplelow i pp
    | otherwise = multiplelow i pp

-- loweestele will take a Poly and return the lowest Integer in it
lowestele :: Poly -> Integer
lowestele [null] = null
lowestele (p:pp:ppp)
-- if the current lowest number (head) is <= pp then keep p as the head and continue recursion with p:ppp
    | p <= pp = lowestele (p:ppp)
-- if the current lowst number(head) > pp, then repalace tyhe head with pp and continue recursion with pp:ppp
    | otherwise = lowestele (pp:ppp)

--removeele will take a Poly (p:pp) and Integer i and remove all i (including duplicates) present in (p:pp)
removeele :: Poly -> Integer -> Poly
removeele [] _ = []
removeele (p:pp) i
-- if the element p /= i, then add [p] to the new list and recurse with the tail pp and i
    | p /= i = [p] ++ removeele pp i
-- if the element p == i, then donot add [p] to the new list and continue recursion with tail pp and i
    | p == i = removeele pp i
-------------------------------------------------------------------------------
--Question 2b

--splitlist will take a [Integer] (i:ii:iii) and return ([all the elements in odd index of (i:ii:iii)],[all the element in even index of (i:ii:iii)])
splitList :: [Integer] -> ([Integer], [Integer])
--splitlist of empty set is a pair of empty set
splitList [] = ([],[])
--splitlist of a single element is that element and a empty list
splitList [i] = ([i],[])
-- split the list into a pair where the left one is the odd indexes and the right one is the even indexes 
splitList (i:ii:iii) = ([i] ++ i1, [ii] ++ ii1) where (i1, ii1) = splitList iii
-------------------------------------------------------------------------------
--Question 2c

--mSort will take in [Integer] p and split it till its element length 1 and merge it all together and return it
mSort :: [Integer] -> [Integer]
mSort [] = []
mSort (p) =  merging (concattion (sorting p))

--sorting will take the [Integer] from mSort and return [([Integer],[Integer])] where all the elements are length 1 or lower and are elements of [Integer]
sorting :: [Integer] -> [([Integer],[Integer])]
sorting [] = [([],[])]
sorting i
-- while the length i > 1 continue recursion
    | length i > 1 = sorting i1 ++ sorting i2
-- if length i == 1 then return [(i,[])]
    | otherwise = [(i,[])]
    where (i1,i2) = splitList i

--concattion will take in the [([Integer],[Integer])] from sorting and concatiante the left and right element in each pair turning them into [Integer] and return a [[Integer]] filled with them
concattion ::  [([Integer],[Integer])] -> [[Integer]]
concattion [([a],_)] = [[a]]
concattion ((x,y):xx) =  [x] ++ concattion xx 

--merging will take [[Integer]] from concattion and will return [Integer] that is made when [[Integer]] is recursively merged using mergeLists
merging :: [[Integer]] -> [Integer]
merging [] = []
merging [a] = a
--merge all the [Integer] in  [[Integer]] recursively until its length 1
merging (i:ii:iii) = merging([(mergeLists i ii)] ++ iii )