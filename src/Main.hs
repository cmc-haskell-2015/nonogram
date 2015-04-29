import Graphics.Gloss.Interface.Pure.Game
import Control.Applicative
------------------------------
type TwoInt = (Int, Int)
type NumTable = [[Int]]
type Board = [[CellState]]
data CellState = No | X | O deriving (Eq, Show)
type Automaton = [(Int, CellState, Int)]
data Field = Field Board NumTable NumTable [Automaton] [Automaton] TwoInt Int Int Float Bool [[Char]]
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

--автоматическое решение--
autoSolutionG :: Field -> Field
autoSolutionG (Field board numt1 numt2 tab1 tab2 r a b sc flag o) = (Field (boardAutoSolution board tab1 numt1) numt1 numt2 tab1 tab2 r a b sc flag o) 
--------------------------
autoSolutionGP :: Field -> Field
autoSolutionGP (Field board numt1 numt2 tab1 tab2 r a b sc flag o) = (Field ((boardAutoSolution (take 45 board) (take 45 tab1) (take 45 numt1)) ++ (drop 45 board)) numt1 numt2 tab1 tab2 r a b sc flag o) 

autoSolutionV :: Field -> Field
autoSolutionV (Field board numt1 numt2 tab1 tab2 r a b sc flag o) = (Field (transpose (boardAutoSolution (transpose board) tab2 numt2)) numt1 numt2 tab1 tab2 r a b sc flag o)
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
renderer (Field world table1 table2 _ _ (i, j) a b sc flag z) = pictures [scale sc sc (pictures[paintBoard world (fromIntegral (15 + (j  * (-15)))::Float) (fromIntegral ((i  * 15) - 15)::Float),
													  								translate (fromIntegral ((j  * (-15)) - (a * 15))::Float) (fromIntegral ((i  * 15) + (b * 15))::Float) (rectangleWire (fromIntegral (a * 30)::Float) (fromIntegral (b * 30)::Float)),
													    							paintTable1 table1 (fromIntegral (j * (-15) + 15 - (a * 30))::Float) (fromIntegral (i * 15 - 15)::Float),
													    							paintTable2 table2 (fromIntegral (j * (-15) + 15)::Float) (fromIntegral (i * 15 - 15 + (b * 30))::Float)]),
																					finishTable flag]

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
-------------------------------------------------------------------------------------------------------------------
--updater не трогаем---
updater _ (Field board table1 table2 numt1 numt2 r a b sc flag z) = (Field board table1 table2 numt1 numt2 r a b sc (checkToWin board numt1) z)
-----------------------
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