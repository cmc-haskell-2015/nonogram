module Logic where

type Size = (Int, Int)
type Pos = (Float, Float)
type NumGroup = [Int]
type NumPanel = [NumGroup]
type Board 	= [[Cell]]
type Automaton = [(Int, Cell, Int)]

data Cell = Empty | No | X | O deriving (Eq, Show)
data SolverAuto = Auto
	{ leftAutomat :: [Automaton]
	, topAutomat  :: [Automaton]
	}

data Field = Field
	{ board    	  :: Board
  	, boardSize   :: Size
  	, leftPanel   :: NumPanel
  	, topPanel	  :: NumPanel
  	, panelSize   :: Size
  	-- SOLVER
  	, solverAuto  :: SolverAuto
  	-- GameOverFlag
  	, gameOver	  :: Bool
  	}
--функция создания поля
createField :: String -> Field
createField contents 
	= Field { board       = emptyBoard n m
					, boardSize   = (n, m)
					, leftPanel   = left
					, topPanel    = top
					, panelSize   = (i, j)
					, solverAuto  = Auto 	{ leftAutomat = lAuto
																, topAutomat  = tAuto
																}
					, gameOver    = False
					}
	where
		(s:ss)  = ll
		ll			= lines contents
		n 	   	= length ll
		m 			= length s
		left 		= createNumpanel leftList i
		top 		= createNumpanel topList j 
		leftList= createNumList ll
		topList	= createNumList (transpose ll)
		i 			= maximum (map length leftList)
		j				= maximum (map length topList)
		lAuto   = map checkLast (map (foldl createAutomat [(1, X, 1)]) left)
		tAuto		= map checkLast (map (foldl createAutomat [(1, X, 1)]) top)

--функция генерации пустого игрового поля
emptyBoard :: Int -> Int -> Board
emptyBoard n m = [ [ No | i <- [1..m] ] | j <- [1..n] ]
--функция создания списка из списков чисел для NUMTABLE 
createNumList :: [String] -> NumPanel
createNumList x = map delLastNull (map (foldl createNumListGroup [0]) x)
--дополнительная функция к createNumList
createNumListGroup :: [Int] -> Char -> [Int]
createNumListGroup x y
	|	and [last x == 0, y == '-'] = x
	|	y == '-' 										= init x ++ [last x, 0]
	|	otherwise 									= init x ++ [last x + 1]
--функция создания NumPanel дополнением каждого списка чисел с переди нулями
createNumpanel :: NumPanel -> Int -> NumPanel
createNumpanel [] _ = []
createNumpanel (x:xs) len 
	= ((replicate (len - length x) 0) ++ x) : createNumpanel xs len
--в процессе CreateNumList в конце каждого списка может появиться лишний 0
--данная фунция удаляет этот несущественный 0
delLastNull :: NumGroup -> NumGroup
delLastNull x 	|	and [last x == 0, length x /= 1]	= init x
								|	otherwise 												= x
--функция транпонирования матрицы(списка списков)
transpose :: [[a]] -> [[a]]
transpose ([] : xs) = []
transpose x = (map head x) : transpose (map tail x)
--функция создания автомата (с помощью свертки в CreateField)
createAutomat :: Automaton -> Int -> Automaton
createAutomat a i 
	|	i /= 0    = a ++ zipWith f l1 l2 ++ l3
	|	otherwise = a
	where
		l1 = [k..(t - 1)]
		l2 = [(k + 1)..t]
		l3 = [(t, X, t + 1), (t + 1, X, t + 1)]
		(l, m, k) = last a
		t = k + i
		f = (\x y -> (x, O, y))
--проверка последнего звена автомата,
--добавление замыкания в себя по X вместо последнего звена 
checkLast :: Automaton -> Automaton
checkLast x |	length x == 1 = x
						|	otherwise 	  = init a ++ [(k, l, k)]
		 				where
		 					a = init x
		 					(k, l, m) = last a
-- перевод всех No и X  в Empty
-- это нужно для дальнейшей отрисовки мини Игрового поля 
oBoard :: Board -> Board
oBoard x = map (map xNoToEmpty) x
-- дополнительная функция к oBoard
xNoToEmpty :: Cell -> Cell
xNoToEmpty x | x == O    = x
			 			 | otherwise = Empty
--функция перевода списка Int-ов в список Float-ов
intToFloat :: [Int] -> [Float]
intToFloat [] = []
intToFloat (x:xs) = (read (show x)::Float) : intToFloat xs
-- вставка нужного нам состояния в (i, j) -ю 
--  позицию на игровой доске (в списке списков)
putCellToBoard :: (Int, Int) -> Board -> Cell -> Board
putCellToBoard _ [] _ = []
putCellToBoard (i, j) board cell
 	|	findedCell == cell = myHead ++ newLineWithNo ++ myTail
	|	otherwise		   		 = myHead ++ newLine ++ myTail
	where
		myHead 				= take i board
		myTail 				= drop (i + 1) board
		findedLine		= head (drop i board)
		myHeadInLine 	= take j findedLine
		myTailInLine 	= drop (j + 1) findedLine
		findedCell 		= head (drop j findedLine)
		newLine 			= [myHeadInLine ++ [cell] ++ myTailInLine]
		newLineWithNo = [myHeadInLine ++ [No] ++ myTailInLine]
--функция для определения нужного коэффициента увеличения(уменьшения) 
scGlob :: (Int, Int) -> Float
scGlob (i, j) = 50.0 / mm
	where
		[mm] = intToFloat [max i j] 
