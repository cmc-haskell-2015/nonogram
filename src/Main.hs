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
				, gameOver  = gameOver
				} 
	= pictures [translate xx yy (scale scGlob scGlob world), line[(300,0),(-300,0)],line[(0,-300),(0,300)]]
	where
		world		  = pictures [boardpic, leftTopRect, miniPic, leftPic, topPic]
		boardpic      = drawBoard board (i, j)
		leftTopRect   = translate (x / 2) (y / 2) (rectangleWire (-x) y)
		miniPic       = translate x y miniBoard
		leftPic       = translate x 0 left'
		topPic        = translate 0 y top'
		left'  		  = drawLeftNumPanel left (i, n)
		top'   		  = drawTopNumPanel top (m, i)
		miniBoard     = scale sx sy (drawBoard (oBoard board) (i, j))
		[fn,fj,fm,fi] = intToFloat [n,j,m,i]
		sx = fn / fj
		sy = fm / fi
		x  = fn * (-10.0)
		y  = fm * 10.0
		xx = fj * (-5.0 * scGlob)
		yy = fi * (5.0 * scGlob)

scGlob :: Float
scGlob = 3.0
		

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
	| and [x >= -sizeX, x <= sizeX, y >= -sizeY, y <= sizeY] 
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
			sizeX = fj * scGlob * 5.0
			sizeY = fi * scGlob * 5.0 
			ii = i - truncate ((y + sizeY) / (10.0 * scGlob)) - 1
			jj = truncate ((x + sizeX) / (10.0 * scGlob))

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
	| and [x >= -sizeX, x <= sizeX, y >= -sizeY, y <= sizeY] 
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
			sizeX = fj * scGlob * 5.0
			sizeY = fi * scGlob * 5.0 
			ii = i - truncate ((y + sizeY) / (10.0 * scGlob)) - 1
			jj = truncate ((x + sizeX) / (10.0 * scGlob))

handler (EventKey (Char 'i') Down _ (x, y)) 
	Field 	{ board       = board
			, boardSize   = (i, j)
			, leftPanel   = left
			, topPanel    = top
			, leftAutomat = lAuto
			, topAutomat  = tAuto
			, panelSize   = panelSize
			, gameOver    = flag
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
			, gameOver    = flag
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
			, gameOver    = flag
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
handler _ field = field

updater _ field = field
