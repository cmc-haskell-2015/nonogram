import Graphics.Gloss.Interface.Pure.Game
import Control.Applicative

import Solver
import Logic

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
--------------------------------------
emptyBoard :: Int -> Int -> Board
emptyBoard n m = [ [ No | i <- [1..n] ] | j <- [1..m] ] 
--Функция создания таблицы с числами
createNumTable :: [[Int]] -> Int -> [[Int]]
createNumTable [] _ = []
createNumTable (a : xs) len
	|	(length a) == len = a : createNumTable xs len
	|	otherwise = ((replicate (len - (length a)) 0) ++ a) : createNumTable xs len
---------------------------------------------------------

maxSize :: [[Int]] -> Int -> Int
maxSize [] maximum = maximum
maxSize (x : xs) maximum
	|	length x > maximum = maxSize xs (length x)
	| 	otherwise = maxSize xs maximum

makeTables :: [[Char]] -> [[Int]]
makeTables [] = []
makeTables (x : xs) = makeTableLine x 0 : makeTables xs

makeTableLine :: [Char] -> Int -> [Int]
makeTableLine [] s |	s /= 0 = [s]
				   |	otherwise = []
makeTableLine (x : xs) s |	(&&) (x == '-') (s /= 0) = s : makeTableLine xs 0
						 |	x == '-' 				 = makeTableLine xs 0
						 |	x == '#' 				 = makeTableLine xs (s + 1)

createField :: [[Char]] -> IO ()
createField [] = putStrLn "ERROR! file: NULL"
createField x = createFieldDop (makeTables x) (makeTables (transpose1 x)) (length x) (length (head x)) x

myScale :: Int -> Float
myScale a = (read (show 20)::Float) / (read (show a)::Float) 
----------------------------------------------------------------------------------------------------
-----------------------ФУНКЦИЙ ДЛЯ РИСОВАНИЯ -------------------------------------------------------
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
paintTableColumn2 :: [Int] -> Float -> Float -> Picture
paintTableColumn2 [] _ _ = blank
paintTableColumn2 (xx : xs) x y =  pictures[paintElem xx x y, paintTableColumn2 xs x (y - 30)]
-------------------------------------------------------------------
---Функция для рисования левой тавлицы с числами2---
paintTable2 :: NumTable -> Float -> Float -> Picture
paintTable2 [] _ _ = blank
paintTable2 (xx : xs) x y = pictures[paintTableColumn2 xx x y, paintTable2 xs (x + 30) y]
-- Дополнительная функция для paintTable1
paintTableRow1 :: [Int] -> Float -> Float -> Picture
paintTableRow1 [] _ _ = blank
paintTableRow1 (xx : xs) x y = pictures[paintElem xx x y, paintTableRow1 xs (x + 30) y]
---Функция для рисования левой тавлицы с числами1---
paintTable1 :: NumTable -> Float -> Float  -> Picture
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
paintBoard (xx : xs) x y = pictures[paintRow xx x y, paintBoard xs x (y - 30)]
----------------------------------------------------------------------------------------------------
-----------------------ФУНКЦИЙ ДЛЯ РИСОВАНИЯ END-------------------------------------------------------
----------------------------------------------------------------------------------------------------

----------------------------
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
renderer (Field world table1 table2 _ _ (i, j) a b sc flag z) = scale sc sc (pictures [boardPic, leftTopRect, leftPic, topPic, finishTable flag])
  where
    boardPic = paintBoard world (fromIntegral (15 + (j  * (-15)))::Float) (fromIntegral ((i  * 15) - 15)::Float)
    leftPic  = paintTable1 table1 (fromIntegral (j * (-15) + 15 - (a * 30))::Float) (fromIntegral (i * 15 - 15)::Float)
    topPic   = paintTable2 table2 (fromIntegral (j * (-15) + 15)::Float) (fromIntegral (i * 15 - 15 + (b * 30))::Float)
    leftTopRect = translate (fromIntegral ((j  * (-15)) - (a * 15))::Float) (fromIntegral ((i  * 15) + (b * 15))::Float) (rectangleWire (fromIntegral (a * 30)::Float) (fromIntegral (b * 30)::Float))


finishTable :: Bool -> Picture
finishTable a |	a == False = blank
			  | otherwise = translate 0 (-250) (scale 0.5 0.5 (pictures [color red (rectangleSolid 400 200),
			  											  				translate (-185) 0 (scale 0.3 0.3 (color white (text "CONGRATULATIONS!!!")))]))
-----------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------Управление действиями--------------------------------------------------
handler (EventKey (MouseButton LeftButton) Down _ (x, y)) (Field world a b t1 t2 (i, j) ms mk sc flag z)
		|	(checkingCoordToField x y ((fromIntegral (j * 15)::Float) * sc) ((fromIntegral (i * 15)::Float) * sc)) = (Field 
					(puttingCellState world (i - (div (truncate y + (i * (truncate (sc / 0.06666666)))) (truncate (sc / 0.03333333))) - 1) (div (truncate x + (j * (truncate (sc / 0.06666666)))) (truncate (sc / 0.03333333))) O)
					a b
					t1 t2
					(i,j)
					ms mk sc flag z)
handler (EventKey (MouseButton RightButton) Down _ (x, y)) (Field world a b t1 t2 (i, j) ms mk sc flag z)
		|	(checkingCoordToField x y ((fromIntegral (j * 15)::Float) * sc) ((fromIntegral (i * 15)::Float) * sc)) = (Field 
					(puttingCellState world (i - (div (truncate y + (i * (truncate (sc / 0.06666666)))) (truncate (sc / 0.03333333))) - 1) (div (truncate x + (j * (truncate (sc / 0.06666666)))) (truncate (sc / 0.03333333))) X)
					a b
					t1 t2
					(i,j)
					ms mk sc flag z)
handler (EventKey (Char 'g') Down _ _ ) (Field board numt1 numt2 tab1 tab2 (i, j) a b sc flag z) = autoSS (Field (emptyBoard j i) numt1 numt2 tab1 tab2 (i, j) a b sc flag z) 
handler (EventKey (Char 's') Down _ _ ) (Field board numt1 numt2 tab1 tab2 (i, j) a b sc flag z) = autoSolutionSpecial (Field (emptyBoard j i) numt1 numt2 tab1 tab2 (i, j) a b sc flag z) 
handler (EventKey (Char 'i') Down _ _ ) world = autoSolutionG world
handler (EventKey (Char 'j') Down _ _ ) world = autoSolutionV world
handler _ world = world

-------------------------------------------------------------------------------------------------------------------
--updater не трогаем---
updater _ (Field board table1 table2 numt1 numt2 r a b sc flag z) = (Field board table1 table2 numt1 numt2 r a b sc (checkToWin board numt1) z)
-----------------------
-- Дополнительная функция для createField
createFieldDop :: [[Int]] -> [[Int]] -> Int -> Int -> [[Char]] -> IO ()
createFieldDop [] [] _ _ _ = putStrLn "ERROR! file: No Field!"
createFieldDop x y i j z = startGame
	(Field
		(emptyBoard j i)
		tab1
		tab2
		(createMatrixAuto tab1)
		(createMatrixAuto tab2)
		(i, j)
		(maxSize x 1)
		(maxSize y 1)
		((read (show 15)::Float) / (read (show (min i j))::Float))
		False
		z
	)
		where tab1 = (createNumTable x (maxSize x 1));
			  tab2 = (createNumTable y (maxSize y 1))
