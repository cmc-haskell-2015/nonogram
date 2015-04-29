module Solver where

import Logic


autoSS :: Field -> Field
autoSS (Field board numt1 numt2 tab1 tab2 (i, j) a b sc flag z) | flag == False = autoSS (autoSolutionV (autoSolutionG  (Field board numt1 numt2 tab1 tab2 (i, j) a b sc (checkToWin board tab1) z)))
															    | otherwise = (Field board numt1 numt2 tab1 tab2 (i, j) a b sc flag z)

autoSolutionSpecial :: Field -> Field
autoSolutionSpecial (Field board numt1 numt2 tab1 tab2 (i, j) a b sc flag z) = (Field (special board z) numt1 numt2 tab1 tab2 (i, j) a b sc flag z)
															--  | otherwise = (Field board numt1 numt2 tab1 tab2 (i, j) a b sc flag)

special :: Board -> [[Char]] -> Board
special [] _ = []
special (x : xs) (y : ys) = specialLine x y : special xs ys

specialLine :: [CellState] -> [Char] -> [CellState] 
specialLine [] _ = []
specialLine (x : xs) (y : ys) |	y == '#' = O : specialLine xs ys
							  | y == '-' = X : specialLine xs ys 


--автоматическое решение--
autoSolutionG :: Field -> Field
autoSolutionG (Field board numt1 numt2 tab1 tab2 r a b sc flag o) = (Field (boardAutoSolution board tab1 numt1) numt1 numt2 tab1 tab2 r a b sc flag o) 
--------------------------
autoSolutionGP :: Field -> Field
autoSolutionGP (Field board numt1 numt2 tab1 tab2 r a b sc flag o) = (Field ((boardAutoSolution (take 45 board) (take 45 tab1) (take 45 numt1)) ++ (drop 45 board)) numt1 numt2 tab1 tab2 r a b sc flag o) 

autoSolutionV :: Field -> Field
autoSolutionV (Field board numt1 numt2 tab1 tab2 r a b sc flag o) = (Field (transpose (boardAutoSolution (transpose board) tab2 numt2)) numt1 numt2 tab1 tab2 r a b sc flag o)

notNullElem :: [Int] -> Int
notNullElem [] = 0
notNullElem (0 : xs) = notNullElem xs
notNullElem (x : xs) = 1 + notNullElem xs

maxElem :: [Int] -> Int -> Int
maxElem [] mm = mm
maxElem (x : xs) mm | x > mm = maxElem xs x
					| otherwise = maxElem xs mm

checkToEdit :: Int -> [Int] -> Bool
checkToEdit len x |	(len - (notNullElem x - 1 + (foldl (+) 0 x))) > (maxElem x 0) = True
				  | otherwise = False

boardAutoSolution :: Board -> [Automaton] -> [[Int]]-> Board
boardAutoSolution [] _ _ = []
boardAutoSolution (x : xs) (y : ys) (z : zs)
	|	length y == 1 = (replicate (length x) X) : boardAutoSolution xs ys zs
	|	(&&) (notNullElem z == 1) (length x == last z) = (replicate (length x) O) : boardAutoSolution xs ys zs
	|	(&&) (x == replicate (length x) No) (checkToEdit (length x) z) = x : boardAutoSolution xs ys zs
	|	otherwise = (linechange x y 0) : boardAutoSolution xs ys zs

createAutomat :: [Int] -> Automaton
createAutomat a | a == (replicate (length a) 0) = [(1, X, 1)]
				| otherwise = (1, X, 1) : (createAutomat1 1 a)

createAutomat1 :: Int -> [Int] -> Automaton
createAutomat1 _ [] = [] 
createAutomat1 a (0 : xs) = createAutomat1 a xs
createAutomat1 a [x] = createAutoDop3 a x
createAutomat1 a (x : xs) = (createAutoDop2 a x) ++ createAutomat1 (a + x + 1) xs

createAutoDop2 :: Int -> Int -> Automaton
createAutoDop2 f 0 = [(f, X, f + 1), (f + 1, X, f + 1)]
createAutoDop2 f c = (f, O, f + 1) : createAutoDop2 (f + 1) (c - 1) 

createAutoDop3 :: Int -> Int -> Automaton
createAutoDop3 f 0 = [(f, X, f)]
createAutoDop3 f c = (f, O, f + 1) : createAutoDop3 (f + 1) (c - 1) 

getNesting :: [CellState] -> [Int] -> Automaton -> [Int]
getNesting [] s  _ = s 
getNesting (x : xs) s a = getNesting xs (nextNesting s x a) a  

nextNesting :: [Int] -> CellState -> Automaton -> [Int]
nextNesting [] _ _ = []
nextNesting (x : xs) s a = nextNestingDop x s a ++ nextNesting xs s a

nextNestingDop :: Int -> CellState -> Automaton -> [Int]
nextNestingDop x s [] = []
nextNestingDop x s ((a, b, c) : ys)
	|	s == No = nextNestingDop x O ((a, b, c) : ys) ++ nextNestingDop x X ((a, b ,c) : ys)
	|	(&&) (x == a) (s == b) = c : nextNestingDop x s ys
	|	otherwise = nextNestingDop x s ys

lastVertex :: Automaton -> Int
lastVertex [] = 0
lastVertex a =  lastDop (last a)

lastDop :: (Int, CellState, Int) -> Int
lastDop (a, b, c) = a

myMember :: Int -> [Int] -> Bool
myMember a [] = False
myMember a (x : xs) |	a == x = True
					|	otherwise = myMember a xs 

checkString :: [CellState] -> Automaton -> Bool
checkString s a | myMember (lastVertex a) (getNesting s [1] a) = True
				| otherwise = False

--
createMatrixAuto :: NumTable -> [Automaton]
createMatrixAuto [] = []
createMatrixAuto (x : xs) = createAutomat x : createMatrixAuto xs

checkXO :: [CellState] -> Automaton -> Int -> [CellState]
checkXO s a i
	|	(&&) (checkString (putCellStateToLine s i O) a == False) (checkString (putCellStateToLine s i X) a == True) = linechange (putCellStateToLine s i X) a (i + 1)
	|	(&&) (checkString (putCellStateToLine s i O) a == True) (checkString (putCellStateToLine s i X) a == False) = linechange (putCellStateToLine s i O) a (i + 1)
	| 	otherwise = linechange s a (i + 1)


linechange :: [CellState] -> Automaton -> Int -> [CellState]
linechange s a i
	|	i >= length s = s 
	|	getCellStateInLine s i == No = checkXO s a i
	|	otherwise = linechange s a (i + 1)

transpose1 :: [[Char]] -> [[Char]]
transpose1 ([] : xs) = []
transpose1 x = ((map head x) : (transpose1 (map tail x)))


transpose :: Board -> Board
transpose ([]:xs) = []
transpose x = ((map head x) : (transpose (map tail x)))

checkToWinLine :: [CellState] -> Automaton -> Bool
checkToWinLine s a  | myMember (lastVertex a) (getNestingWin s [1] a) = True
					| otherwise = False

getNestingWin :: [CellState] -> [Int] -> Automaton -> [Int]
getNestingWin [] s  _ = s 
getNestingWin (x : xs) s a = getNestingWin xs (nextNestingWin s x a) a  

nextNestingWin :: [Int] -> CellState -> Automaton -> [Int]
nextNestingWin [] _ _ = []
nextNestingWin (x : xs) s a = nextNestingDop1 x s a ++ nextNestingWin xs s a

nextNestingDop1 :: Int -> CellState -> Automaton -> [Int]
nextNestingDop1 x s [] = []
nextNestingDop1 x s ((a, b, c) : ys)
	|	s == No = nextNestingDop1 x X ((a, b ,c) : ys)
	|	(&&) (x == a) (s == b) = c : nextNestingDop1 x s ys
	|	otherwise = nextNestingDop1 x s ys

checkToWin :: Board -> [Automaton] -> Bool
checkToWin [] _ = True
checkToWin (x : xs) (y : ys) = (&&) (checkToWinLine x y) (checkToWin xs ys) 
