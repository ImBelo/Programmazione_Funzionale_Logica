import Data.List (group, sort)

-- Definizione del tipo per la griglia
type Grid = [(Int, Int)]

-- Funzione per calcolare i vicini di una cella
neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x+dx, y+dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

-- Funzione per contare i vicini vivi di una cella
countLiveNeighbors :: Grid -> (Int, Int) -> Int
countLiveNeighbors grid cell = length $ filter (`elem` grid) (neighbors cell)

-- Funzione per determinare le celle che sopravvivono
survivors :: Grid -> Grid
survivors grid = [cell | cell <- grid, let n = countLiveNeighbors grid cell, n == 2 || n == 3]

-- Funzione per determinare le nuove nascite
births :: Grid -> Grid
births grid = [cell | cell <- unique (concatMap neighbors grid), cell `notElem` grid, countLiveNeighbors grid cell == 3]
  where unique = map head . group . sort

-- Funzione per evolvere la griglia al passo successivo
nextGeneration :: Grid -> Grid
nextGeneration grid = survivors grid ++ births grid

-- Funzione per visualizzare la griglia
showGrid :: Grid -> String
showGrid grid = unlines [[if (x, y) `elem` grid then '#' else '.' | x <- [minX..maxX]] | y <- [minY..maxY]]
  where
    minX = minimum (map fst grid)
    maxX = maximum (map fst grid)
    minY = minimum (map snd grid)
    maxY = maximum (map snd grid)

-- Esempio di griglia iniziale (pattern: "glider")
initialGrid :: Grid
initialGrid = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]

-- Funzione principale per eseguire il gioco
main :: IO ()
main = do
  let generations = iterate nextGeneration initialGrid
  mapM_ (\gen -> putStrLn (showGrid gen) >> putStrLn "") (take 10 generations)
