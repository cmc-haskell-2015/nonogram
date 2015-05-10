module Solver where

import Logic
------------------------------------------------------
----------------- ссылка на алгоритм -----------------
-- http://www.ict.edu.ru/ft/005765/2007_3_57-65.pdf --
------------------------------------------------------
-- функция которая последовательно применяет 
-- авторешатель по строкам и авторешатель по столбцам
-- до тех пор, пока игра не закончится(checkToWin == True)
-- либо количество итерации перевалит за порогорое значение (10)
autoGen :: Board -> NumPanel -> NumPanel -> [Automaton] -> [Automaton] -> Int -> Board
autoGen board left top lAuto tAuto i
	| or [i > 10, checkToWin board lAuto] = board
	| otherwise = autoGen columns left top lAuto tAuto (i + 1)
	where
		columns = transpose (autoSolution trows top tAuto)
		trows 	= transpose rows
		rows 		= autoSolution board left lAuto
-- авторешатель по строкам (по столбцам, если заранее транспонировать игровую доску)
autoSolution :: Board -> NumPanel -> [Automaton] -> Board
autoSolution board panel autoList 
	= map checkAndChangeLine (zip3 board panel autoList)
-- проверка на тревиальные случаи(когда автоматы не нужны и можно заполнить клетки)
checkAndChangeLine :: ([Cell], NumGroup, Automaton) -> [Cell]
checkAndChangeLine (cells, numbers, automat)	
	|	lA == 1 																		= replicate lC X
	|	numbers == [0 | i <- [1..(lN - 1)]] ++ [lC] = replicate lC O
	|	otherwise = autoSolutionLine cells automat 0
	where
		lC = length cells
		lN = length numbers
		lA = length automat
-- прогон по клеткам строки и автомату, проверка на то, заполнина ли уже клетка
autoSolutionLine :: [Cell] -> Automaton -> Int -> [Cell]
autoSolutionLine cells automat i |	i >= length cells = cells
	|	iCell == No = autoSolutionLine nextStepCells automat (i + 1)
	|	otherwise 	= autoSolutionLine cells automat (i + 1)
	where
		iCell 				= head (drop i cells)
		nextStepCells = insertAndCheck cells automat i
-- проверка на возможные случаи
--т.е пробуем положить в i-ю клетку O, затем X,
-- если возможен только один исход, то заполняем клетку
insertAndCheck :: [Cell] -> Automaton -> Int -> [Cell]
insertAndCheck cells automat i 
	|	and [ checkNewLine newCelllWithO automat
				, not (checkNewLine newCelllWithX automat)
				] 
				= newCelllWithO
	|	and [checkNewLine newCelllWithX	automat
				, not (checkNewLine newCelllWithO automat)
				] 
				= newCelllWithX
	|	otherwise = cells
	where
		newCelllWithO = myHead ++ [O] ++ myTail
		newCelllWithX = myHead ++ [X] ++ myTail
		myHead 				= take i cells
		myTail				= drop (i + 1) cells
-- проверка полученной строки на возможность
-- то есть есть ли такое дальнейшее доЗаполнение всех остальных клеток,
-- так что полученная строка удволитворяла бы условиям
checkNewLine :: [Cell] -> Automaton -> Bool
checkNewLine cells automat
	|	elem ll finishNesting = True
	|	otherwise 					  = False
	where
		k = last automat
		(j, p, ll) = k
		finishNesting = foldl nextNesting [1] sp
		sp = map (\x -> (x, automat)) cells
-- получение по входным вершинам автомата и исходным данным
-- множество следующих вершин 
nextNesting :: [Int] -> (Cell, Automaton) -> [Int]
nextNesting [] _ = []
nextNesting (x : xs) (s, a)
	= nextNestingDop x s a ++ nextNesting xs (s, a)
-- дополнительная функция к nextNesting
nextNestingDop :: Int -> Cell -> Automaton -> [Int]
nextNestingDop x s [] = []
nextNestingDop x s ((a, b, c) : ys)
	|	s == No 						 = nO ++ nX
	|	and [x == a, s == b] = c : nextNestingDop x s ys
	|	otherwise 					 = nextNestingDop x s ys
	where
		nO = nextNestingDop x O ((a, b, c) : ys)
		nX = nextNestingDop x X ((a, b ,c) : ys)
-- функция провери на завершение Игры  
checkToWin :: Board -> [Automaton] -> Bool
checkToWin board automats 
	= and (map checkToWinLine (zip board automats))
-- дополнительная функция к chekToWin
checkToWinLine :: ([Cell], Automaton) -> Bool
checkToWinLine (cells, automat) 
	= checkNewLine (map changeNoToX cells) automat
-- дополнительная функция к chekToWin
-- которая заменяет в игровой доске все элементы No на X
changeNoToX :: Cell -> Cell
changeNoToX a |	a == No 	= X
			 				| otherwise = a
