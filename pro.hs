import Graphics.Gloss.Interface.Pure.Game
import Data.Map
import Data.Set

data CellState = Nothing | Da | Net

type Field = Map Cell CellState
type Cell = (Int, Int)

events (EventKey (MouseButton LeftButton) Down _ (x, y)) _ = pictures [translate x y (color black (rectangleSolid 30 30)),
                                                                        table]
events _ _ = table

main :: IO ()
main =  do
	let field = createField
	startGame field

createField :: Field
createField = Data.Map.empty

startGame :: Field -> IO ()
startGame _ = play 
	(InWindow "GameEvent" (600, 600) (10, 10))
    green
    100
    (Circle 10.0)
    id
    events
    (\_ world -> world)

both f (a, b) = (f a, f b)

renderer _ = pictures [table, translate 0 0 (circleSolid 50)]
table = pictures [uncurry translate (cellToScreen (x, y)) $ color black $ rectangleWire 30 30 
		| x <- [(-6) .. 6], y <- [(-6) .. 6]]

cellToScreen = both ((* 30) . fromIntegral)
handler (EventKey (MouseButton LeftButton) Down _ (x, y)) _ = translate x y (color black (rectangleSolid 30 30))
updater _ world = world



