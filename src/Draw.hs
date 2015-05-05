module Draw where

import Graphics.Gloss.Interface.Pure.Game

import Logic

drawLeftNumPanel :: NumPanel -> Size -> Picture
drawLeftNumPanel left size = pictures (map (drawNum) (zip (createPosMatrix size) (concat left))) 

drawTopNumPanel :: NumPanel -> Size -> Picture
drawTopNumPanel top size = pictures (map (drawNum) (zip (createPosMatrix size) (concat (transposeInt top)))) 

drawBoard :: Board -> Size -> Picture
drawBoard board size = pictures (map (drawCell) (zip (createPosMatrix size) (concat board)))

drawNum :: (Pos, Int) -> Picture
drawNum ((x, y), 0) = rect x y
drawNum ((x, y), a) = pictures [ numb, rect x y]
	where 
		numb = translate (x - 4) (y - 3) (scale 0.04 0.03 (text (show a)))

rect :: Float -> Float -> Picture
rect x y = translate x y (rectangleWire 10 10)

drawCell :: (Pos, Cell) -> Picture
drawCell ((x, y), c) 	|	c == No    = rect x y
				  		|	c == X     = pictures [ cross, rect x y]
				  		|	c == Empty = blank
				  		|	otherwise  = translate x y (rectangleSolid 10 10) 
				 		 where
				  			cross = pictures[ line [(x + 5, y + 5), (x - 5, y - 5)]
				  							, line [(x + 5, y - 5), (x - 5, y + 5)]
				  							]

createPosMatrix :: Size -> [Pos]
createPosMatrix (i, j) = map (fromIntToFloat) (concat (map (zip [0..(j - 1)]) (map (replicate j) [0..(i - 1)])))

fromIntToFloat :: (Int, Int) -> Pos
fromIntToFloat (x, y) = (read (show (x * 10 + 5))::Float, read (show (y * (-10) - 5))::Float)
