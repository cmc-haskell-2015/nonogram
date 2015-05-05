module Logic where

type Size = (Int, Int)

type Pos = (Float, Float)

data Cell = Empty | No | X | O deriving (Eq, Show)

type NumGroup = [Int]

type NumPanel = [NumGroup]

type Board 	= [[Cell]]

type Automaton = [(Int, Cell, Int)]

data Field = Field
	{ board    	  :: Board
  	, boardSize   :: Size
  	, leftPanel   :: NumPanel
  	, topPanel	  :: NumPanel
  	, leftAutomat :: [Automaton]
  	, topAutomat  :: [Automaton]
  	, panelSize   :: Size
  	, gameOver	  :: Bool
  	}

createField :: String -> Field
createField contents 
	= Field { board       = emptyBoard n m
			, boardSize   = (n, m)
			, leftPanel   = left
			, topPanel    = top
			, leftAutomat = lAuto
			, topAutomat  = tAuto
			, panelSize   = (i, j)
			, gameOver    = False
			}
	where
		(s:ss)	= lines contents
		n 	   	= length (s:ss)
		m 		= length s
		left 	= createNumpanel leftList i
		top 	= createNumpanel topList j 
		leftList= createNumList (s:ss)
		topList	= createNumList (transpose (s:ss))
		i 		= maximum (map (length) leftList)
		j		= maximum (map (length) topList)
		lAuto   = map (checkLast) (map (foldl (createAutomat) [(1, X, 1)]) left)
		tAuto	= map (checkLast) (map (foldl (createAutomat) [(1, X, 1)]) top)


emptyBoard :: Int -> Int -> Board
emptyBoard n m = [ [ No | i <- [1..n] ] | j <- [1..m] ]

createNumList :: [String] -> NumPanel
createNumList x = map (delLastNull) (map (foldl (createNumListGroup) [0]) x)

createNumListGroup :: [Int] -> Char -> [Int]
createNumListGroup x y 	|	and [last x == 0, y == '-'] = x
						|	y == '-' 					= init x ++ [last x, 0]
						|	otherwise 					= init x ++ [last x + 1]

createNumpanel :: NumPanel -> Int -> NumPanel
createNumpanel [] _ = []
createNumpanel (x:xs) len = ((replicate (len - length x) 0) ++ x) : createNumpanel xs len

delLastNull :: NumGroup -> NumGroup
delLastNull x 	|	and [last x == 0, length x /= 1] = init x
				|	otherwise 	= x

transpose :: [[a]] -> [[a]]
transpose ([] : xs) = []
transpose x = ((map head x) : (transpose (map tail x)))

transposeInt :: [[Int]] -> [[Int]]
transposeInt ([] : xs) = []
transposeInt x = ((map head x) : (transposeInt (map tail x)))


createAutomat :: Automaton -> Int -> Automaton
createAutomat a i 
	|	i /= 0    = a ++ (zipWith f [k..(t - 1)] [(k + 1)..t] ++ [(t, X, t + 1), (t + 1, X, t + 1)])
	|	otherwise = a
	where
		(l, m, k) = (last a)
		t = k + i
		f = (\x y -> (x, O, y))

checkLast :: Automaton -> Automaton
checkLast x |	length x == 1 = x
		 	|	otherwise = init a ++ [(k, l, k)]
		 		where
		 			a = init x
		 			(k, l, m) = last a

oBoard :: Board -> Board
oBoard x = map (map (xNoToEmpty)) x

xNoToEmpty :: Cell -> Cell
xNoToEmpty x | x == O = x
			 | otherwise = Empty

intToFloat :: [Int] -> [Float]
intToFloat [] = []
intToFloat (x:xs) = (read (show x)::Float) : intToFloat xs

putCellToBoard :: (Int, Int) -> Board -> Cell -> Board
putCellToBoard _ [] _ = []
putCellToBoard (i, j) board cell 	|	findedCell == cell = myHead ++ newLineWithNo ++ myTail
									|	otherwise =  myHead ++ newLine ++ myTail
	where
	myHead = take i board
		myTail = drop (i + 1) board
		findedLine = head (drop i board)
		myHeadInLine = take j findedLine
		myTailInLine = drop (j + 1) findedLine
		findedCell = head (drop j findedLine)
		newLine = [myHeadInLine ++ [cell] ++ myTailInLine]
		newLineWithNo = [myHeadInLine ++ [No] ++ myTailInLine]
