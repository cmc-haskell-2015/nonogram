module Main where

import Graphics.Gloss.Interface.Pure.Game

import Control.Applicative

import Logic

import Solver

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

renderer Field  { board 	= board
				, boardSize = (i, j)
				, leftPanel = left
				, topPanel  = top
				, panelSize = (n, m)
				, gameOver  = flag
				} 
	= translate xx yy (scale (scGlob (i, j)) (scGlob (i, j)) world)
	where
		world		  = pictures [boardpic, leftTopRect, miniPic, leftPic, topPic, gameOverPic]
		boardpic      = drawBoard board (i, j)
		leftTopRect   = translate (x / 2) (y / 2) (rectangleWire (-x) y)
		miniPic       = translate x y miniBoard
		leftPic       = translate x 0 left'
		topPic        = translate 0 y top'
		gameOverPic   = (drawGameOverPic flag (fj * 5) (-fi * 5))
		left'  		  = drawLeftNumPanel left (i, n)
		top'   		  = drawTopNumPanel top (m, j)
		miniBoard     = scale sx sy (drawBoard (oBoard board) (i, j))
		[fn,fj,fm,fi] = intToFloat [n,j,m,i]
		sx = fn / fj
		sy = fm / fi
		x  = fn * (-10.0)
		y  = fm * 10.0
		xx = fj * (-5.0 * scGlob (i, j))
		yy = fi * (5.0 * scGlob (i, j))

scGlob :: (Int, Int) -> Float
scGlob (i, j) = 50.0 / mm
	where
		[mm] = intToFloat [max i j] 

		

--ЛЕВАЯ КНОПКА МЫШИ
handler (EventKey (MouseButton LeftButton) Down _ (x, y)) 
	Field 	{ board       = board
			, boardSize   = (i, j)
			, leftPanel   = left
			, topPanel    = top
			, leftAutomat = lAuto
			, topAutomat  = tAuto
			, panelSize   = panelSize
			, gameOver    = gameOver
			}
	| and [x > -sizeX, x < sizeX, y > -sizeY, y < sizeY] 
		= Field { board 	  = putCellToBoard (ii, jj) board O
				, boardSize   = (i, j)
				, leftPanel   = left
				, topPanel    = top
				, leftAutomat = lAuto
				, topAutomat  = tAuto
				, panelSize   = panelSize
				, gameOver    = gameOver
				}
		where
			[fj, fi] = intToFloat [j, i]
			sizeX = fj * scGlob (i, j) * 5.0
			sizeY = fi * scGlob (i, j) * 5.0 
			ii = - truncate ((y - sizeY) / (10.0 * scGlob (i, j)))
			jj = truncate ((x + sizeX) / (10.0 * scGlob (i, j)))

--ПРАВАЯ КНОПКА МЫШИ
handler (EventKey (MouseButton RightButton) Down _ (x, y)) 
	Field 	{ board       = board
			, boardSize   = (i, j)
			, leftPanel   = left
			, topPanel    = top
			, leftAutomat = lAuto
			, topAutomat  = tAuto
			, panelSize   = panelSize
			, gameOver    = gameOver
			}
	| and [x > -sizeX, x < sizeX, y > -sizeY, y < sizeY] 
		= Field { board 	  = putCellToBoard (ii, jj) board X
				, boardSize   = (i, j)
				, leftPanel   = left
				, topPanel    = top
				, leftAutomat = lAuto
				, topAutomat  = tAuto
				, panelSize   = panelSize
				, gameOver    = gameOver
				}
		where
			[fj, fi] = intToFloat [j, i]
			sizeX = fj * (scGlob (i, j)) * 5.0
			sizeY = fi * (scGlob (i, j)) * 5.0 
			ii = - truncate ((y - sizeY) / (10.0 * scGlob (i, j)))
			jj = truncate ((x + sizeX) / (10.0 * scGlob (i, j)))

handler (EventKey (Char 'i') Down _ _) 
	Field 	{ board       = board
			, boardSize   = (i, j)
			, leftPanel   = left
			, topPanel    = top
			, leftAutomat = lAuto
			, topAutomat  = tAuto
			, panelSize   = panelSize
			, gameOver    = False
			}
	= Field { board       = nextStepBoard
			, boardSize   = (i, j)
			, leftPanel   = left
			, topPanel    = top
			, leftAutomat = lAuto
			, topAutomat  = tAuto
			, panelSize   = panelSize
			, gameOver    = newFlag
			}
	where
		newFlag = checkToWin nextStepBoard lAuto
		nextStepBoard = autoSolution board left lAuto

handler (EventKey (Char 'j') Down _ _) 
	Field 	{ board       = board
			, boardSize   = (i, j)
			, leftPanel   = left
			, topPanel    = top
			, leftAutomat = lAuto
			, topAutomat  = tAuto
			, panelSize   = panelSize
			, gameOver    = False
			}
	= Field { board       = nextStepBoard
			, boardSize   = (i, j)
			, leftPanel   = left
			, topPanel    = top
			, leftAutomat = lAuto
			, topAutomat  = tAuto
			, panelSize   = panelSize
			, gameOver    = newFlag
			}
	where
		newFlag = checkToWin nextStepBoard lAuto
		nextStepBoard = transpose (autoSolution (transpose board) top tAuto)

handler (EventKey (Char 'g') Down _ _) 
	Field 	{ board       = board
			, boardSize   = (i, j)
			, leftPanel   = left
			, topPanel    = top
			, leftAutomat = lAuto
			, topAutomat  = tAuto
			, panelSize   = panelSize
			, gameOver    = False
			}
	= Field { board       = finishBoard
			, boardSize   = (i, j)
			, leftPanel   = left
			, topPanel    = top
			, leftAutomat = lAuto
			, topAutomat  = tAuto
			, panelSize   = panelSize
			, gameOver    = newFlag
			}
	where
		newFlag = checkToWin finishBoard lAuto
		finishBoard = autoGen (emptyBoard i j) left top lAuto tAuto 0

handler (EventKey (Char 'r') Down _ _) 
	Field 	{ board       = board
			, boardSize   = (i, j)
			, leftPanel   = left
			, topPanel    = top
			, leftAutomat = lAuto
			, topAutomat  = tAuto
			, panelSize   = panelSize
			, gameOver    = flag
			}
	= Field { board       = emptyBoard i j
			, boardSize   = (i, j)
			, leftPanel   = left
			, topPanel    = top
			, leftAutomat = lAuto
			, topAutomat  = tAuto
			, panelSize   = panelSize
			, gameOver    = False
			}
			
handler _ field = field

updater _ Field { board       = board
				, boardSize   = (i, j)
				, leftPanel   = left
				, topPanel    = top
				, leftAutomat = lAuto
				, topAutomat  = tAuto
				, panelSize   = panelSize
				, gameOver    = flag
				}
	= Field { board       = board
			, boardSize   = (i, j)
			, leftPanel   = left
			, topPanel    = top
			, leftAutomat = lAuto
			, topAutomat  = tAuto
			, panelSize   = panelSize
			, gameOver    = checkToWin board lAuto
			}
