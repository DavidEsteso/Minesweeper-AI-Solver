module Minesweeper where

import qualified Data.Map.Strict as Map
import System.Random (randomRIO)
import Control.Monad 
import Data.List ((\\))

type Position = (Int, Int)
type MineField = Map.Map Position CellContent
data BoardSize = BoardSize { 
    boardWidth :: Int, 
    boardHeight :: Int 
} deriving (Show, Eq)

data CellContent = CellContent {
    content :: CellType,
    cellState :: CellState
} deriving (Show, Eq)

data CellType = Mine | Empty Int
    deriving (Show, Eq)

data CellState = Hidden | Revealed | Flagged | QuestionMark
    deriving (Show, Eq)

data GameState = NotStarted | Playing | Won | Lost
    deriving (Show, Eq)

data Game = Game {
    gameBoard :: MineField,
    gameSize :: BoardSize,
    mineCount :: Int,
    gameState :: GameState
} deriving (Show, Eq)


-- Crear un nuevo juego
newGame :: Int -> Int -> Int -> IO Game
newGame width height mines = do
    let size = BoardSize width height
        emptyBoard = Map.fromList [((x, y), CellContent (Empty 0) Hidden) 
                                 | x <- [0..width-1], y <- [0..height-1]]
    return $ Game {
        gameBoard = emptyBoard,
        gameSize = size,
        mineCount = mines,
        gameState = NotStarted
    }

initializeBoard :: Game -> Position -> IO Game
initializeBoard game firstPos = do
    let size = gameSize game
        width = boardWidth size
        height = boardHeight size
        mines = mineCount game
        allPositions = [(x, y) | x <- [0..width-1], y <- [0..height-1]]
        -- Exclude the first position and its neighbors from mine placement
        safePositions = firstPos : getAdjacentPositions size firstPos
        validMinePositions = allPositions \\ safePositions
    
    minePositions <- generateMinePositions validMinePositions mines
    let board = createBoard size minePositions
    return $ game {
        gameBoard = board,
        gameState = Playing
    }

-- Generar posiciones de minas de manera eficiente
generateMinePositions :: [Position] -> Int -> IO [Position]
generateMinePositions validPositions mineCount = do
    let total = length validPositions
    pickMines mineCount []
  where
    pickMines 0 acc = return acc
    pickMines n acc = do
        i <- randomRIO (0, length validPositions - 1)
        let chosen = validPositions !! i
        if chosen `elem` acc
           then pickMines n acc -- Retry if the position is already chosen
           else pickMines (n - 1) (chosen : acc)

-- Creates a new Minesweeper board with mines placed at specified positions
-- and calculates the number of adjacent mines for each empty cell
createBoard :: BoardSize -> [Position] -> MineField
createBoard size minePositions = 
    -- First, create a list of all valid board positions
    let allPositions = generateAllBoardPositions size
        
        -- Then create the initial board with mines and empty cells
        initialBoard = createInitialBoard allPositions minePositions
        
        -- Finally, calculate the numbers for empty cells
        finalBoard = calculateNumbers size initialBoard
    in finalBoard

-- Calculte the numbers based on the adjacent mines
calculateNumbers :: BoardSize -> MineField -> MineField
calculateNumbers size board = Map.mapWithKey updateCell board
  where
    updateCell pos cell = case content cell of
        Mine    -> cell
        Empty _ -> cell { content = Empty (countAdjacentMines pos) }

    countAdjacentMines pos = length $ filter isMine (getAdjacentPositions size pos)

    isMine pos = case getCellAt pos board of
        Just cell -> content cell == Mine
        Nothing   -> False


-- Check if the game is over
checkWinCondition :: Game -> Bool
checkWinCondition game =
    let board = gameBoard game
        totalCells = boardWidth (gameSize game) * boardHeight (gameSize game)
        mineCount' = mineCount game
        revealedCount = Map.size $ Map.filter isRevealed board
    in revealedCount == (totalCells - mineCount')

-- Reveals a cell on the board
revealCell :: Game -> Position -> IO (Game, [Position])
revealCell game pos
    | not (isValidPosition (gameSize game) pos) = return (game, [])
    | gameState game == Lost || gameState game == Won = return (game, [])
    | isFlagged game pos = return (game, [])
    | gameState game == NotStarted = do
        initializedGame <- initializeBoard game pos
        return $ revealCellCore initializedGame pos
    | otherwise = return $ revealCellCore game pos

-- Reveals empty cells recursively
revealEmpty :: Game -> [Position] -> MineField -> (MineField, [Position])
revealEmpty game [] board = (board, [])
revealEmpty game (currentPos:remainingPositions) board = 
    case getCellAt currentPos board of
        Nothing -> 
            revealEmpty game remainingPositions board
        Just cell -> 
            case (content cell, cellState cell) of
                -- If the cell is empty and hidden, reveal it and continue with its neighbors
                (Empty 0, Hidden) -> 
                    let updatedBoard = Map.insert currentPos (cell { cellState = Revealed }) board
                        neighborPositions = getAdjacentPositions (gameSize game) currentPos
                        newPositionsQueue = neighborPositions ++ remainingPositions
                        (finalBoard, positions) = revealEmpty game newPositionsQueue updatedBoard
                    in (finalBoard, currentPos : positions)
                -- If the cell is a number and hidden, reveal it and stop
                (Empty _, Hidden) -> 
                    let updatedBoard = Map.insert currentPos (cell { cellState = Revealed }) board
                        (finalBoard, positions) = revealEmpty game remainingPositions updatedBoard
                    in (finalBoard, currentPos : positions)
                _ -> 
                    revealEmpty game remainingPositions board

-- Logic to reveal a cell
revealCellCore :: Game -> Position -> (Game, [Position])
revealCellCore game position = 
    case getCellAt position (gameBoard game) of
        Nothing -> 
            (game, [])
        Just cell -> 
            case (content cell, cellState cell) of
                (_, Revealed) -> 
                    (game, [])
                (_, Flagged) -> 
                    (game, [])
                -- If the cell is a mine, the game is lost
                (Mine, _) -> 
                    (game { gameState = Lost }, [position])
                -- If the cell is a number, reveal it
                (Empty adjacentMines, Hidden) -> 
                    let (updatedBoard, changedPositions) = 
                            if adjacentMines == 0
                                then revealEmpty game [position] (gameBoard game)
                                else let board = Map.insert position (cell { cellState = Revealed }) (gameBoard game)
                                     in (board, [position])
                        updatedGame = game { gameBoard = updatedBoard }
                        finalGame = if checkWinCondition updatedGame
                                  then updatedGame { gameState = Won }
                                  else updatedGame
                    in (finalGame, changedPositions)

-- Rotate the cell state (Hidden -> Flagged -> QuestionMark -> Hidden)
toggleFlag :: Game -> Position -> (Game, [Position])
toggleFlag game pos = 
    case getCellAt pos (gameBoard game) of
        Just cell -> case cellState cell of
            Hidden -> 
                (game { gameBoard = Map.insert pos (cell { cellState = Flagged }) (gameBoard game) }, [pos])
            Flagged -> 
                (game { gameBoard = Map.insert pos (cell { cellState = QuestionMark }) (gameBoard game) }, [pos])
            QuestionMark -> 
                (game { gameBoard = Map.insert pos (cell { cellState = Hidden }) (gameBoard game) }, [pos])
            Revealed -> 
                (game, [])
        Nothing -> (game, [])

-- Count the number of flagged neighbors
countFlaggedNeighbors :: Game -> Position -> Int
countFlaggedNeighbors game pos = 
    let neighbors = getAdjacentPositions (gameSize game) pos
        countFlags = go neighbors 0
    in countFlags
    where
        go [] acc = acc
        go (p:ps) acc = 
            if isFlagged game p
            then go ps (acc + 1)
            else go ps acc

-- Handle click on a number
handleChordClick :: Game -> Position -> IO (Game, [Position])
handleChordClick game pos
    | gameState game /= Playing = return (game, [])
    | not ( isValidPosition (gameSize game) pos) = return (game, [])
    | otherwise = case getCellAt pos (gameBoard game) of
        -- A revelealed number where the chord can be applied
        Just (CellContent (Empty n) Revealed) ->
            -- The chord condition: the number of flagged neighbors is equal to the number of the cell
            if countFlaggedNeighbors game pos == n 
            then do
                let neighbors = getAdjacentPositions (gameSize game) pos
                    unflaggedNeighbors = filter (not . isFlagged game) neighbors
                revealCells game unflaggedNeighbors
            else return (game, [])
        _ -> return (game, [])

-- Reveals all remaining cells when the game is over (win or lose)
revealRemaining :: Game -> IO (Game, [Position])
revealRemaining game = do
    if areAllMinesCorrectlyMarked game
    then do
        let board = gameBoard game
            hiddenPositions = [pos | (pos, cell) <- Map.toList board, 
                                   cellState cell == Hidden]
            revealedBoard = Map.map (\cell -> cell { cellState = Revealed }) board
            finalGame = game { gameBoard = revealedBoard, gameState = Won }
        return (finalGame, hiddenPositions)
    else
        return (game { gameState = Lost }, [])



----------------------------------- HELPER FUNCTIONS -----------------------------------

-- Helper function to generate all valid positions on the board
generateAllBoardPositions :: BoardSize -> [Position]
generateAllBoardPositions size = 
    [ (x, y) 
    | x <- [0..boardWidth size - 1]
    , y <- [0..boardHeight size - 1]
    ]

-- Helper function to create the initial board with mines placed
createInitialBoard :: [Position] -> [Position] -> MineField
createInitialBoard allPositions minePositions = 
    Map.fromList $ map createCell allPositions
    where
        createCell pos = 
            if pos `elem` minePositions
            then (pos, CellContent Mine Hidden)      -- Place a mine
            else (pos, CellContent (Empty 0) Hidden) -- Place an empty cell


-- Helper function to get the adjacent positions of a given position
getAdjacentPositions :: BoardSize -> Position -> [Position]
getAdjacentPositions (BoardSize w h) (x, y) = 
    [(nx, ny) | nx <- [x-1..x+1], ny <- [y-1..y+1],
                nx >= 0, ny >= 0, nx < w, ny < h,
                (nx, ny) /= (x, y)]

-- Helper function to count the number of flagged cells
flagCount :: [(Position, CellContent)] -> Int
flagCount [] = 0
flagCount ((_, cell) : xs) =
    if cellState cell == Flagged
    then 1 + flagCount xs
    else flagCount xs

-- Helper function to get the cell content at a given position
getCellAt :: Position -> MineField -> Maybe CellContent
getCellAt = Map.lookup

-- Helper function to call revealCell on multiple positions
revealCells :: Game -> [Position] -> IO (Game, [Position])
revealCells game [] = return (game, [])
revealCells game (p:ps) = do
    (newGame, positions1) <- revealCell game p
    (finalGame, positions2) <- revealCells newGame ps
    return (finalGame, positions1 ++ positions2)

-- Helper function to check if a position is valid
isValidPosition :: BoardSize -> Position -> Bool
isValidPosition (BoardSize w h) (x, y) = 
    x >= 0 && x < w && y >= 0 && y < h

-- Helper function to check if a cell is flagged
isFlagged :: Game -> Position -> Bool
isFlagged game pos = 
    case getCellAt pos (gameBoard game) of
        Just cell -> cellState cell == Flagged
        Nothing -> False

-- Helper function to check if a cell is revealed
isRevealed :: CellContent -> Bool
isRevealed (CellContent _ Revealed) = True
isRevealed _ = False

-- Helper to check if a cell is a number
isNumber :: CellType -> Bool
isNumber (Empty n) = True
isNumber Mine = False

-- Helper to ckeck if a cell is hidden or marked with a question mark
isHidden :: Game -> Position -> Bool 
isHidden game pos = 
    case getCellAt pos (gameBoard game) of
        Just cell -> cellState cell == Hidden
        Nothing -> False

-- Helper function to get all hidden cells on the board
getAllHiddenCells :: Game -> [Position]
getAllHiddenCells game = 
    [ pos 
    | (pos, cell) <- Map.toList (gameBoard game)
    , isHidden game pos
    ]

-- getRevealedCells
getRevealedCells :: Game -> [Position]
getRevealedCells game = 
    [ pos 
    | (pos, cell) <- Map.toList (gameBoard game)
    , isRevealed cell
    ]
    
-- Helper function to check if all mines are correctly marked
areAllMinesCorrectlyMarked :: Game -> Bool
areAllMinesCorrectlyMarked game =
    let board = gameBoard game
        isFlaggedMine (_, CellContent Mine Flagged) = True
        isFlaggedMine _ = False
        flaggedMines = length $ filter isFlaggedMine $ Map.toList board
    in flaggedMines == mineCount game

-- Helper function to count the remaining mines
countRemainingMines :: Game -> Int
countRemainingMines game =
    let board = gameBoard game
        flaggedCount = flagCount (Map.toList board)
    in mineCount game - flaggedCount

-- Helper to get the mine count of a cell
getMineCount :: CellType -> Int
getMineCount (Empty n) = n
getMineCount Mine = 0