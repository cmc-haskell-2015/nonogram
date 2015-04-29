module Logic where

------------------------------
type TwoInt = (Int, Int)
type NumTable = [[Int]]
type Board = [[CellState]]
data CellState = No | X | O deriving (Eq, Show)
type Automaton = [(Int, CellState, Int)]
data Field = Field Board NumTable NumTable [Automaton] [Automaton] TwoInt Int Int Float Bool [[Char]]

--------------------------------------------------------
----------Проверяем координаты на пренадлежность полю-----------
checkingCoordToField :: Float -> Float -> Float -> Float -> Bool
checkingCoordToField x y j i  	| (&&) ((&&) (x >= (-j)) (x <= j)) ((&&) (y >= (-i)) (y <= i)) = True
                          		| otherwise = False
---------------------------------------------------
--Функция вытаскивающая состояние (i,j)-ой клетки
getCellState :: Board -> Int -> Int -> CellState
getCellState [] _ _ = No
getCellState (x : xs) 0 j = getCellStateInLine x j
getCellState (x : xs) i j = getCellState xs (i - 1) j
-----------------------------------------------------
-- Дополнительная функция для getCellState
getCellStateInLine :: [CellState] -> Int -> CellState
getCellStateInLine [] _ = No
getCellStateInLine (x : xs) 0 = x
getCellStateInLine (x : xs) j = getCellStateInLine xs (j - 1)
-------------------------------------------------------------
--Функция вставляющая заданное состояние в (i,j)-ую клетку поля
putCellStateToWorld :: Board -> Int -> Int -> CellState-> Board
putCellStateToWorld [] _ _ _ = []
putCellStateToWorld (x : xs) 0 j s = (putCellStateToLine x j s) : xs
putCellStateToWorld (x : xs) i j s = x : (putCellStateToWorld xs (i - 1) j s)
-----------------------------------------------------------------------------
-- Дополнительная функция для putCellStateToWorld
putCellStateToLine :: [CellState] -> Int -> CellState -> [CellState]
putCellStateToLine [] _ _ = []
putCellStateToLine (x : xs) 0 s = s : xs
putCellStateToLine (x : xs) j s = x : (putCellStateToLine xs (j - 1) s)
-----------------------------------------------------------------------
----Функция вставляющая заданное состояние в (i,j)-ую клетку поля
--------- после проверки нынешнего состояния клетки
puttingCellState :: Board -> Int -> Int -> CellState -> Board
puttingCellState [] _ _ _ = []
puttingCellState world i j s  | (getCellState world i j) == No  = (putCellStateToWorld world i j s)
                              | (getCellState world i j) == s   = (putCellStateToWorld world i j No)
                              | otherwise                       = world 
