--refrence used: https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-List.html#v:concat

module A1 where

-- Question 1
--myLog takes in an integer b and integer x  and return log base(x) b
myLog :: Integer -> Integer -> Integer
--returns log base _ of 1 = 0
myLog _ 1 = 0
-- returns log base b of x = 1
myLog b x  = 1 + myLog b (div x b)

-- Question 2
type Person = String
type Book = String
type Database = [ (Person, [Book])]

-- books takes a database dtb and person per and returns the books loaned by the person per  
books:: Database -> Person -> [Book]
books dtb pr
--returns the books loaned by person per
    = concat [ b | (p, b) <- dtb , p == pr  ]  

-- borrowers takes in a databse dtb and book bk and returns loaners of book bk
borrowers :: Database -> Book -> [Person]
borrowers dtb bk
--returns the people who have borrowed book bk
    = [p| (p,b) <- dtb, (elem (bk) b ) == True ]

-- borrowed takes in a databse dtb and Book bk and return if anyone borrowed bk
borrowed :: Database -> Book -> Bool
borrowed dtb bk
-- calls the function borrowers, and if the list of person /= [], then returns True, otherwise false
    | borrowers dtb bk /= [] = True
    | otherwise = False

--numBorrowed takes in databse dtb and person per to return number of books loaned by per
numBorrowed :: Database -> Person -> Int
-- returns the number of books loaned by per
numBorrowed dtb per = sum[1| (p,b) <- dtb, bb <- b, per ==p ]

-- filter 2 is a helper function for filter3 function that that takes a list of book b and book bk and returns b without bk
filter2:: [Book] -> Book -> [Book]
-- if bk is an element of b, it removes bk from b and return b
filter2 b bk = [bb| bb<- b, bb /= bk]

--filter 3 is a helper function for removeLoan that takes input databse dtb, person per, list of books, b and book bk and returns a database with updated pairs
filter3:: Database -> Person -> [Book] -> Book -> Database
filter3 dtb per b bk
-- if bk is an element of b and removing bk from b /= [], then return database dtb such that it contains tuple (per, b without bk) while also removing (per,b) from dtb
    | filter2 b bk /= [] = (per, filter2 (books dtb per) bk) : [(p,b) | (p,b) <- dtb, p /= per] 
-- bk removed from b == [], then the person has no more loaned boooks, remove person per form dtb
    | otherwise = [(p,b) | (p,b) <- dtb, p /= per] 

-- makeLoan takes input database dtb, person per and book bk to return a databse with the updated book list
makeLoan :: Database -> Person -> Book -> Database
makeLoan dtb per bk 
-- if no book bk was given, return untouched dtb
    | bk == [] = dtb
-- if bk is not being loaned by person per then add tuple (per, [ list of his pre-owned book ++ bk]) and remove the tuple (per, [preowned books]) from dtb
    | elem per (borrowers dtb bk) /= True =  (per, (books dtb per) ++ [bk]) : [(p,b) | (p,b) <- dtb, p /= per]
-- if the book was loaned but per already owned it, return untouched dtb
    | otherwise = dtb

--returnLoan takes input databse dtb, person per, and book bk to return a databse with the updated book list
returnLoan :: Database -> Person -> Book -> Database
returnLoan dtb per bk
-- if no book bk was given, return dtb
    | bk == [] = dtb
-- if person per owns book bk, then call filter3 and return an updated database
    | (elem per (borrowers dtb bk)) == True =  filter3 dtb per (books dtb per) bk
-- if the book was given but was never owned by per , return dtb
    | otherwise = dtb

-- Question 3
type Picture = [[Char]]

-- row scale takes array of char and int as input to return a scaled version of the elements in the row
row_scale:: [Char] -> Int -> [Char]
-- if the [char] ==[], then do not scale
row_scale [] _ = []
row_scale c i
-- opens the [char] and makes i copies of each element and returns [char]
    = concat [replicate i cc | cc <- c]

-- column scale takes a picture and int and returns a scaled version of the columns
column_scale:: Picture -> Int -> Picture
-- if the picture ==[], then do not scale
column_scale [] _ = []
column_scale (p) i
--  opens the picture and makes i copies of each [char] and returns picture
    = concat[ replicate i pp | pp <- p]

-- scale takes a picture p and int i and returns a picture p scaled by i
scale:: Picture -> Int -> Picture
scale p i
-- multiplies the elements by i using row_scale and then passes it on to column_scale where it multiples columns by i to return a scaled pic
    = column_scale [row_scale pp i | pp <- p] i

--Question 4
type Graph = [(Int, Int)]

-- helper function that takes graph g int i and int i2 and returns a list of friends of i when g = [(i,i2)]
friend1 :: Graph -> Int -> Int -> [Int]
friend1 g i i2
-- checks if g = (f, f2) and f == i such that i \= i2
    =[ f2 |( f, f2) <- g, i /= i2 && i == f ]

-- helper function that takes graph g int i and int i2 and returns a list of friends of i when g = [(i2,i)]
friend11 :: Graph -> Int -> Int -> [Int]
-- checks if g = (f, f2) and f2 == i such that i \= i2
friend11 g i i2
    = [f | (f, f2) <- g, i /= i2 && i == f2]

-- helper function that combines the friends of i in friend1 and friend11 and returns them
combine :: Graph -> Int -> Int -> [Int]
combine g i i1
-- returns the combination of the return of friend11 and the return of friend1 such that the same friend isnt included multiple times in the list
    = filt(friend1 g i i1) ( friend11 g i i1) ++ friend11 g i i1

-- takes [Int] i and [Int] ii and returns [Int] such that i does not contain the same friends as ii
filt :: [Int] -> [Int] -> [Int]
filt i ii 
    = [iii| iii<- i , elem iii ii /= True]

--takes input graph g, int i and int i1 and returns  [int] of all their common friends
commonFriends:: Graph -> Int -> Int -> [Int]
commonFriends g i i1
-- if the graph /= [], combine the common friends of i and i1 
    | g /= [] = [ l | l <- (combine g i i1), elem l (combine g i1 i) == True]
-- if graph is empty then return []
    | otherwise = []