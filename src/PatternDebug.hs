module PatternDebug where

import Minesweeper
import MinesweeperSolver
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

-- Function to create a test board with a pattern of mines
createTestBoard :: IO Game
createTestBoard = do
    let width = 12
        height = 12
        mines = 8
        game = Game
          { gameBoard = createTestPattern
          , gameSize = BoardSize width height
          , mineCount = mines
          , gameState = Playing
          }
    return game
  where
    createTestPattern = Map.fromList $ 
      [((x, y), CellContent (Empty 0) Hidden) | x <- [0..11], y <- [0..11]] ++
      
      [((0,0), CellContent (Empty 0) Revealed),
       ((0,1), CellContent (Empty 1) Revealed),
       ((0,2), CellContent (Empty 2) Revealed),
       ((0,3), CellContent (Empty 1) Revealed),
       ((0,4), CellContent (Empty 0) Revealed),
       ((1,0), CellContent (Empty 0) Hidden),
       ((1,1), CellContent (Empty 0) Hidden),
       ((1,2), CellContent (Empty 0) Hidden),
       ((1,3), CellContent (Empty 0) Hidden),
       ((1,4), CellContent (Empty 0) Hidden),
       ((2,0), CellContent (Empty 0) Revealed),
       ((2,1), CellContent (Empty 0) Revealed),
       ((2,2), CellContent (Empty 0) Revealed),
       ((2,3), CellContent (Empty 0) Revealed),
       ((2,4), CellContent (Empty 0) Revealed)] ++


      [((3,0), CellContent (Empty 0) Revealed),
       ((3,1), CellContent (Empty 0) Revealed),
       ((3,2), CellContent (Empty 0) Revealed),
       ((3,3), CellContent (Empty 0) Revealed),
       ((3,4), CellContent (Empty 0) Revealed),
       ((4,0), CellContent (Empty 0) Revealed),
       ((4,1), CellContent (Empty 1) Revealed),
       ((4,2), CellContent (Empty 2) Revealed),
       ((4,3), CellContent (Empty 1) Revealed),
       ((4,4), CellContent (Empty 0) Revealed),
       ((5,0), CellContent (Empty 0) Hidden),
       ((5,1), CellContent (Empty 0) Hidden),
       ((5,2), CellContent (Empty 0) Hidden),
       ((5,3), CellContent (Empty 0) Hidden),
       ((5,4), CellContent (Empty 0) Hidden)] ++


      [((7,0), CellContent (Empty 0) Hidden),
       ((7,1), CellContent (Empty 0) Hidden),
       ((7,2), CellContent (Empty 1) Revealed),
       ((7,3), CellContent (Empty 0) Revealed),
       ((8,0), CellContent (Empty 0) Hidden),
       ((8,1), CellContent (Empty 0) Hidden),
       ((8,2), CellContent (Empty 2) Revealed),
       ((8,3), CellContent (Empty 0) Revealed),
       ((9,0), CellContent (Empty 0) Hidden),
       ((9,1), CellContent (Empty 0) Hidden),
       ((9,2), CellContent (Empty 1) Revealed),
       ((9,3), CellContent (Empty 0) Revealed),
       ((10,3), CellContent (Empty 0) Revealed),
       ((10,2), CellContent (Empty 0) Revealed),
       ((6,2), CellContent (Empty 0) Revealed),
       ((6,3), CellContent (Empty 0) Revealed)] ++


      [((6,6), CellContent (Empty 0) Revealed),  
       ((7,6), CellContent (Empty 1) Revealed),
       ((8,6), CellContent (Empty 2) Revealed),
       ((9,6), CellContent (Empty 1) Revealed),
       ((10,6), CellContent (Empty 0) Revealed), 
       ((7,8), CellContent (Empty 0) Hidden),   
       ((8,8), CellContent (Empty 0) Hidden),
       ((9,8), CellContent (Empty 0) Hidden),
       ((6,5), CellContent (Empty 0) Revealed),  
       ((7,5), CellContent (Empty 0) Revealed),
       ((8,5), CellContent (Empty 0) Revealed),
       ((9,5), CellContent (Empty 0) Revealed),
       ((10,5), CellContent (Empty 0) Revealed)]

-- Function to print the board state in a readable way
printBoard :: Game -> IO ()
printBoard game = do
    let size = gameSize game
        board = gameBoard game
    putStrLn "Current board state:"
    mapM_ (\y -> do
        mapM_ (\x -> printCell (getCellAt (x,y) board)) [0..boardWidth size - 1]
        putStrLn "") [0..boardHeight size - 1]
  where
    printCell Nothing = putStr "? "
    printCell (Just cell) = case (content cell, cellState cell) of
        (Mine, Hidden)   -> putStr "H "
        (Mine, Revealed) -> putStr "* "
        (Mine, Flagged)  -> putStr "F "
        (Empty n, Hidden)   -> putStr "H "
        (Empty n, Revealed) -> putStr (show n ++ " ")
        (Empty n, Flagged)  -> putStr "F "
        (_, QuestionMark) -> putStr "? "

-- Function to test pattern detection
testPatternDetection :: IO ()
testPatternDetection = do
    game <- createTestBoard
    putStrLn "Initial board state:"
    printBoard game
    
    let twos = findRevealedTwos game
    putStrLn $ "Positions with number 2 found: " ++ show twos
    
    putStrLn "\nAnalyzing each position with 2:"
    mapM_ (analyzePattern game) twos
  where
    analyzePattern game pos = do
        putStrLn $ "\nAnalyzing pattern at position: " ++ show pos
        let (horizontalPattern, verticalPattern) = debugFindPatterns game pos
        putStrLn $ "Mines found in the horizontal pattern: " ++ show horizontalPattern
        putStrLn $ "Mines found in the vertical pattern: " ++ show verticalPattern
        let board = gameBoard game
            leftPos = (fst pos - 1, snd pos)
            rightPos = (fst pos + 1, snd pos)
        putStrLn $ "Left cell: " ++ show (getCellAt leftPos board)
        putStrLn $ "Right cell: " ++ show (getCellAt rightPos board)

-- Function to run all debug tests
runDebugTests :: IO ()
runDebugTests = do
    putStrLn "Starting pattern debug tests..."
    testPatternDetection
    putStrLn "\nTests completed."
