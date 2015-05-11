module Main where

import Graphics.Gloss.Interface.Pure.Game
import Control.Applicative

import Nonogram.Logic
import Nonogram.Solver
import Draw

main :: IO ()
main = do
	path  <- getLine
	field <- createField <$> readFile path
	startGame field

startGame :: Field -> IO ()
startGame world 
	= play
	  	(InWindow "JapanCross" (800, 800) (10, 10))
    	green
    	100
    	world
    	renderer              
    	handler
    	updater

-- | Функция для отрисовки всего FIELD.
renderer :: Field -> Picture
renderer Field 	{ board 	  = board
								, boardSize = (i, j)
								, leftPanel = left
								, topPanel  = top
								, panelSize = (n, m)
								, gameOver  = flag
								}
	= translate xx yy (scale worldScale worldScale world)
	where
		world = pictures [boardpic, leftTopRect, miniPic, leftPic, topPic, gameOverPic]
		boardpic      = drawBoard board (i, j)
		leftTopRect   = translate (x / 2) (y / 2) (rectangleWire (-x) y)
		miniPic       = translate x y miniBoard
		leftPic       = translate x 0 left'
		topPic        = translate 0 y top'
		gameOverPic   = (drawGameOverPic flag (fj * 5) (-fi * 5))
		left'  		 		= drawLeftNumPanel left (i, n)
		top'   		  	= drawTopNumPanel top (m, j)
		miniBoard     = scale sx sy (drawBoard (oBoard board) (i, j))
		[fn,fj,fm,fi] = intToFloat [n,j,m,i]
		worldScale	  = scGlob (i, j)
		sx 						= fn / fj
		sy 						= fm / fi
		x  						= fn * (-10.0)
		y  						= fm * 10.0
		xx 						= fj * (-5.0) * worldScale
		yy 						= fi * 5.0 * worldScale

-- | Обрабатываются действия пользователя, в зависимости от них изменяется FIELD.
handler :: Event -> Field -> Field
--обработка нажатия кнопок мыши
handler (EventKey (MouseButton mbutton) Down _ (x, y)) 
	f@Field	{ board     = board
					, boardSize = (i, j)
					}
	| and [x > -sizeX, x < sizeX, y > -sizeY, y < sizeY, mbutton == LeftButton] 
		= f	{ board = putCellToBoard (ii, jj) board O } 
	| and [x > -sizeX, x < sizeX, y > -sizeY, y < sizeY, mbutton == RightButton]
		= f { board = putCellToBoard (ii, jj) board X }
		where
			[fj, fi] = intToFloat [j, i]
			sizeX = fj * scGlob (i, j) * 5.0
			sizeY = fi * scGlob (i, j) * 5.0 
			ii = - truncate ((y - sizeY) / (10.0 * scGlob (i, j)))
			jj = truncate ((x + sizeX) / (10.0 * scGlob (i, j)))
--обработка нажатия клавиш
handler (EventKey (Char key) Down _ _)
	f@Field	{ board       = board
					, boardSize 	= (i, j)
					, leftPanel   = left
					, topPanel 	  = top
					, solverAuto  = Auto { leftAutomat = lAuto
															 , topAutomat  = tAuto }
					}
	| key == 'r' = f { board = emptyBoard i j }
	|	key == 'i' = f { board = nextStepBoardRows }
	|	key == 'j' = f { board = nextStepBoardColumns }
	| key == 'g' = f { board = finishBoard }
	where
		nextStepBoardRows    = autoSolution board left lAuto
		nextStepBoardColumns = transpose (autoSolution (transpose board) top tAuto)
		finishBoard 				 = autoGen (emptyBoard i j) left top lAuto tAuto 0
handler _ field = field

-- | Обновитель мира, который каждый раз обновляет флаг конца игры GameOver.
updater :: Float -> Field -> Field
updater _ f@Field { board       = board
				  				, solverAuto  = Auto { leftAutomat = lAuto }
				 					, gameOver    = flag 
				 					}
	= f { gameOver = checkToWin board lAuto }
