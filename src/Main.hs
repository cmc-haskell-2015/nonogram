module Main where

import Graphics.Gloss.Interface.Pure.Game
import Control.Applicative
------------------------------
type ThreeInt = (Int, Int, Int)
type TwoInt = (Int, Int)
type NumTable = [ThreeInt]
type Board = [[CellState]]
data CellState = No | X | O deriving (Eq, Show)
data Field = Field Board NumTable NumTable TwoInt
-------------------------------------------------
--Функция считывания чисел из строки
getInt :: String -> [Int]
getInt [] = []
getInt x = (getElem (reads x :: [(Int, String)])) : getInt (getString (reads x :: [(Int, String)]))
--------------------------------------------------------------------------------------------------
-- Дополнительная функция для getInt
getElem :: [(Int, String)] -> Int
getElem [] = 0
getElem ((x, y):xs) = x
-- Дополнительная функция для getInt
getString :: [(Int, String)] -> String
getString [] = []
getString ((x, y):xs) = y
------Функция создания пустого поля------
createList :: Int -> Int -> Board
createList n m = createN n (createM m)
--------------------------------------
-- Дополнительная функция для createList
createN :: Int -> [CellState] -> Board
createN 0 _ = []
createN n x = x : (createN (n - 1) x)
-- Дополнительная функция для createList
createM :: Int -> [CellState]
createM 0 = [] 
createM m = No : (createM (m - 1))
--Функция создания таблицы с числами
createNumTable :: [[Int]] -> NumTable
createNumTable [] = []
createNumTable ([a, b, c] : xs) = (a, b, c) : createNumTable xs
createNumTable ([b, c] : xs) = (0, b, c) : createNumTable xs
createNumTable ([c] : xs) = (0, 0, c) : createNumTable xs
---------------------------------------------------------
--Создаем МИР и передаем его в startGame
createField :: [String] -> IO ()
createField [] = putStrLn "ERROR! file: NULL"
createField (x : xs) = createFieldDop xs (head (getInt x)) (last (getInt x))
-- Дополнительная функция для createField
createFieldDop :: [String] -> Int -> Int -> IO ()
createFieldDop [] _ _ = putStrLn "ERROR! file: No Field!"
createFieldDop x i j = startGame
	(Field
		(createList i j)
		(createNumTable (map (getInt) (take i x)))
		(createNumTable (map (getInt) (drop (i + 1) x)))
		(i, j)
	)
--------------------------------------------------------
----------Проверяем координаты на пренадлежность полю-----------
checkingCoordToField :: Int -> Int -> Int -> Int -> Bool
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
----------------------------------------------------------------------------------------------------
--Функция для рисования клетки с числом внутри
paintElem :: Int -> Float -> Float -> Picture
paintElem a x y 
	|	a == 0 = translate x y (rectangleWire 30 30)
	|	div a 10 >= 1 = pictures[translate x y (rectangleWire 30 30),
								translate (x - 11) (y - 5) (scale 0.13 0.1 (text (show a)))]
	|	otherwise = pictures[translate x y (rectangleWire 30 30),
							 translate (x - 7) (y - 5) (scale 0.13 0.1 (text (show a)))]
-------------------------------------------
-- Дополнительная функция для paintTable2
paintTableColumn2 :: ThreeInt -> Float -> Float -> Picture
paintTableColumn2 (a, b, c) x y =  pictures[paintElem a x y,
											paintElem b x (y - 30),
											paintElem c x (y - 60)]
-------------------------------------------------------------------
---Функция для рисования левой тавлицы с числами2---
paintTable2 :: NumTable -> Float -> Float -> Picture
paintTable2 [] _ _ = blank
paintTable2 (xx : xs) x y = pictures[paintTableColumn2 xx x y, paintTable2 xs (x + 30) y]
-- Дополнительная функция для paintTable1
paintTableRow1 :: ThreeInt -> Float -> Float -> Picture
paintTableRow1 (a, b, c) x y = pictures[paintElem a x y,
										paintElem b (x + 30) y,
										paintElem c (x + 60) y]
---Функция для рисования левой тавлицы с числами1---
paintTable1 :: NumTable -> Float -> Float -> Picture
paintTable1 [] _ _ = blank
paintTable1 (xx : xs) x y = pictures[paintTableRow1 xx x y, paintTable1 xs x (y - 30)]
--------------------------------------------------------------------------------------
----------------------------------------------------
paintRow :: [CellState] -> Float -> Float -> Picture
paintRow [] _ _ = blank
paintRow (xx : xs) x y
	|	xx == No = pictures[translate x y (rectangleWire 30 30), paintRow xs (x + 30) y]
	|	xx == O  = pictures[translate x y (rectangleSolid 30 30), paintRow xs (x + 30) y]
	|	xx == X  = pictures[krestik x y, paintRow xs (x + 30) y]
------------------------------------------
krestik :: Float -> Float -> Picture
krestik x y = pictures[(line[(x + 15, y + 15), (x - 15, y - 15)]),
					   (line[(x + 15, y - 15), (x - 15, y + 15)]),
					   translate x y (rectangleWire 30 30)]
------------------------------------------------------------------
--Функция рисования Поля
paintBoard :: Board -> Float -> Float -> Picture
paintBoard [] _ _ = blank
paintBoard (xx : xs) x y = pictures[paintRow xx x y, paintBoard xs x (y + 30)]
--------Начало игры-------
startGame :: Field -> IO ()
startGame startWorld = play 
	  			(InWindow "JapanCross" (600, 600) (10, 10))
    			green
    			100
    			startWorld
    			renderer              
    			handler
    			updater
-----------------------
-------------MAIN-----------------
main :: IO ()
main = do
      file <- getLine
      a <- lines <$> readFile file
      createField a
----------------------------------
renderer (Field world table1 table2 (i, j)) = pictures[paintBoard world (fromIntegral (15 + (j  * (-15)))::Float) (fromIntegral (15 + (i  * (-15)))::Float),
													   translate (fromIntegral ((j  * (-15)) - 45)::Float) (fromIntegral ((i  * 15) + 45)::Float) (rectangleWire 90 90),
													   paintTable1 table1 (fromIntegral (j * (-15) - 75)::Float) (fromIntegral (i * 15 - 15)::Float),
													   paintTable2 table2 (fromIntegral (j * (-15) + 15)::Float) (fromIntegral (i * 15 + 75)::Float)]
-----------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------Управление действиями--------------------------------------------------
handler (EventKey (MouseButton LeftButton) Down _ (x, y)) (Field world a b (i, j))
		|	(checkingCoordToField (round x) (round y) (j * 15) (i * 15)) = (Field 
					(puttingCellState world (div (round y + (i * 15)) 30) (div (round x + (j * 15)) 30) O)
					a b
					(i,j))
handler (EventKey (MouseButton RightButton) Down _ (x, y)) (Field world a b (i, j))
		|	(checkingCoordToField (round x) (round y) (j * 15) (i * 15)) = (Field 
					(puttingCellState world (div (round y + (i * 15)) 30) (div (round x + (j * 15)) 30) X)
					a b
					(i,j))
handler _ world = world
-------------------------------------------------------------------------------------------------------------------
--updater не трогаем---
updater _ world = world
-----------------------