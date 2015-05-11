module Draw where

import Graphics.Gloss.Interface.Pure.Game
import Nonogram.Logic

-- | Отрисовщик левой числовой панели.
drawLeftNumPanel :: NumPanel -> Size -> Picture
drawLeftNumPanel left size = pictures (map drawNum ll)
	where
		ll = zip (createPosMatrix size) (concat left)

-- | Отрисовщик верхней числовой панели.
drawTopNumPanel :: NumPanel -> Size -> Picture
drawTopNumPanel top size 
	= pictures (map drawNum ll)
	where
		ll = zip (createPosMatrix size) (concat (transpose top))

-- | Отрисовщик игровой доски.
drawBoard :: Board -> Size -> Picture
drawBoard board size = pictures (map drawCell ll)
	where
		ll = zip (createPosMatrix size) (concat board)

-- | Отрисовщик числа (вместе с внешней клеткой).
drawNum :: (Pos, Int) -> Picture
drawNum ((x, y), 0) = rect x y
drawNum ((x, y), a) = pictures [ numb, rect x y]
	where 
		numb = translate (x-4) (y-3) (scale 0.04 0.03 (text (show a)))

-- | Стандартная (в моем понимании) клетка.
rect :: Float -> Float -> Picture
rect x y = translate x y (rectangleWire 10 10)

-- | Отрисовщик игровой клетки.
drawCell :: (Pos, Cell) -> Picture
drawCell ((x, y), c) 	
	|	c == No    = rect x y
	|	c == X     = pictures [ cross, rect x y]
	|	c == Empty = blank
	|	otherwise  = translate x y (rectangleSolid 10 10) 
	where
		cross = pictures[ line [(x+5, y+5), (x-5, y-5)]
				  					, line [(x+5, y-5), (x-5, y+5)]
				  					]

-- | Отрисовщик всплывающего по окончанию игры окошка (прямоугольника).
drawGameOverPic :: Bool -> Float -> Float -> Picture
drawGameOverPic flag x y 
	|	flag == False = blank
	|	otherwise 		
		= pictures [ translate x y (color red (rectangleSolid 80 25))
					 		 , translate (x-35) (y-7) myText]
	where
		myText = scale 0.1 0.1 (color white (text "Game Over")) 

-- | Создание матрицы (x, y) координат.
createPosMatrix :: Size -> [Pos]
createPosMatrix (i, j) 
	= map intToFloatShift [ (x,y) | y <- [0..(i - 1)], x <- [0..(j - 1)]]

-- | Перевод координат из Int в Float со смещением.
intToFloatShift :: (Int, Int) -> Pos
intToFloatShift (x, y) = (xx, yy)
	where
		[fx, fy] = intToFloat [x, y]
		xx = fx * 10.0 + 5.0
		yy = fy * (-10.0) - 5.0

