module Solver where

import Logic

autoGen :: Board -> NumPanel -> NumPanel -> [Automaton] -> [Automaton] -> Int -> Board
autoGen board left top lAuto tAuto i | or [i > 10, checkToWin board lAuto] = board
									 | otherwise = autoGen columns left top lAuto tAuto (i + 1)
	where
		columns = transpose (autoSolution trows top tAuto)
		trows = transpose rows
		rows = autoSolution board left lAuto

autoSolution :: Board -> NumPanel -> [Automaton] -> Board
autoSolution board panel autoList = map (checkAndChangeLine) (zip3 board panel autoList)

checkAndChangeLine :: ([Cell], NumGroup, Automaton) -> [Cell]
checkAndChangeLine (cells, numbers, automat)	
	|	lA == 1 = replicate lC X
	|	numbers == [0 | i <- [1..(lN - 1)]] ++ [lC] = replicate lC O
	|	otherwise = autoSolutionLine cells automat 0
	where
		lC = length cells
		lN = length numbers
		lA = length automat

autoSolutionLine :: [Cell] -> Automaton -> Int -> [Cell]
autoSolutionLine cells automat i |	i >= length cells = cells
								 |	iCell == No = autoSolutionLine nextStepCells automat (i + 1)
								 |	otherwise = autoSolutionLine cells automat (i + 1) 
	where
		iCell = head (drop i cells)
		nextStepCells = insertAndCheck cells automat i

insertAndCheck :: [Cell] -> Automaton -> Int -> [Cell]
insertAndCheck cells automat i |	and [ checkNewLine newCelllWithO automat
										, not (checkNewLine newCelllWithX automat)
										] = newCelllWithO
							   |	and [checkNewLine newCelllWithX	automat
							   			, not (checkNewLine newCelllWithO automat)
							   			] = newCelllWithX
							   |	otherwise = cells
	where
		newCelllWithO = myHead ++ [O] ++ myTail
		newCelllWithX = myHead ++ [X] ++ myTail
		myHead = take i cells
		myTail = drop (i + 1) cells

checkNewLine :: [Cell] -> Automaton -> Bool
checkNewLine cells automat |	elem ll finishNesting = True
						   |	otherwise = False
	where
		k = last automat
		(j, p, ll) = k
		finishNesting = foldl (nextNesting) [1] (map (\x -> (x, automat)) cells)

nextNesting :: [Int] -> (Cell, Automaton) -> [Int]
nextNesting [] _ = []
nextNesting (x : xs) (s, a) = nextNestingDop x s a ++ nextNesting xs (s, a)

nextNestingDop :: Int -> Cell -> Automaton -> [Int]
nextNestingDop x s [] = []
nextNestingDop x s ((a, b, c) : ys)
	|	s == No = nextNestingDop x O ((a, b, c) : ys) ++ nextNestingDop x X ((a, b ,c) : ys)
	|	(&&) (x == a) (s == b) = c : nextNestingDop x s ys
	|	otherwise = nextNestingDop x s ys


summ :: NumGroup -> Int
summ numbers = notNullElems numbers + foldl (+) 0 numbers - 1

notNullElems :: NumGroup -> Int
notNullElems [] = 0
notNullElems [0] = 1
notNullElems (x:xs) | x /= 0 = 1 + notNullElems xs
					| otherwise = notNullElems xs

checkToWin :: Board -> [Automaton] -> Bool
checkToWin board automats = and (map (checkToWinLine) (zip board automats))

checkToWinLine :: ([Cell], Automaton) -> Bool
checkToWinLine (cells, automat) = checkNewLine (map (changeNoToX) cells) automat

changeNoToX :: Cell -> Cell
changeNoToX x |	x == No = X
			  | otherwise = x 
--
