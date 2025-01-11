module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.IORef
import Minesweeper
import MinesweeperSolver
import System.Environment (getArgs)
import System.Exit (exitFailure)

data DisplayMode = TimeMode | MovesMode
    deriving (Eq)

type StyleName = String
type StyleProperties = [(String, String)]

data ButtonInfo = ButtonInfo {
    button :: UI.Element,
    styleCache :: Map.Map StyleName StyleProperties
}

-- Constantes globales del juego
defaultBoardWidth :: Int
defaultBoardWidth = 8

defaultBoardHeight :: Int
defaultBoardHeight = 8

defaultMines :: Int
defaultMines = 10

data Difficulty = Easy | Medium | Hard | Custom Int Int Int
    deriving (Show, Read)

difficultySettings :: Difficulty -> (Int, Int, Int)
difficultySettings Easy = (defaultBoardWidth, defaultBoardHeight, defaultMines)
difficultySettings Medium = (16, 16, 40)
difficultySettings Hard = (30, 16, 99)
difficultySettings (Custom w h m) = (w, h, m)

buttonBaseStyle :: [(String, String)]
buttonBaseStyle = [ ("width", "30px")
                 , ("height", "30px")
                 , ("background-color", "#c0c0c0")
                 , ("margin", "0")
                 , ("border-top", "2px solid #ffffff")
                 , ("border-left", "2px solid #ffffff")
                 , ("border-right", "2px solid #7b7b7b")
                 , ("border-bottom", "2px solid #7b7b7b")
                 , ("font-weight", "bold")
                 , ("font-size", "20px")
                 , ("line-height", "16px")
                 , ("padding", "0")
                 , ("text-align", "center")
                 , ("vertical-align", "middle")
                 , ("user-select", "none")
                 , ("-webkit-user-select", "none")
                 ]

wrongFlagStyle :: [(String, String)]
wrongFlagStyle = buttonBaseStyle ++ [("background-color", "#ff0000")]

clickedMineStyle :: [(String, String)]
clickedMineStyle = revealedStyle ++ [("background-color", "#ff0000")]
                 

revealedStyle :: [(String, String)]
revealedStyle = [ ("background-color", "#c0c0c0")
                , ("border", "1px solid #808080")
                , ("border-top-color", "#808080")
                , ("border-left-color", "#808080")
                , ("border-right-color", "#808080")
                , ("border-bottom-color", "#808080")
                ]

gridStyle :: [(String, String)]
gridStyle = [ ("border-collapse", "collapse")
           , ("background-color", "#c0c0c0")
           , ("border-top", "3px solid #ffffff")
           , ("border-left", "3px solid #ffffff")
           , ("border-right", "3px solid #7b7b7b")
           , ("border-bottom", "3px solid #7b7b7b")
           , ("margin", "10px auto")
           , ("padding", "5px")
           ]

timeDisplayStyle :: [(String, String)]
timeDisplayStyle = [ ("font-family", "Digital-7, Courier New, monospace")
                  , ("font-size", "24px")
                  , ("margin", "5px auto")
                  , ("display", "inline-block")
                  , ("text-align", "center")
                  , ("font-weight", "normal")
                  , ("color", "#ff0000")
                  , ("background-color", "#000000")
                  , ("padding", "2px 8px")
                  , ("border-top", "2px solid #7b7b7b")
                  , ("border-left", "2px solid #7b7b7b")
                  , ("border-right", "2px solid #ffffff")
                  , ("border-bottom", "2px solid #ffffff")
                  , ("min-width", "60px")
                  , ("cursor", "pointer")  
                  , ("user-select", "none")  
                  , ("-webkit-user-select", "none")
                  ]

replayButtonStyle :: [(String, String)]
replayButtonStyle = [ ("width", "35px")
                   , ("height", "35px")
                   , ("background-color", "#c0c0c0")
                   , ("margin", "5px auto")
                   , ("border-top", "2px solid #ffffff")
                   , ("border-left", "2px solid #ffffff")
                   , ("border-right", "2px solid #7b7b7b")
                   , ("border-bottom", "2px solid #7b7b7b")
                   , ("cursor", "pointer")
                   , ("display", "block")
                   , ("text-align", "center")
                   , ("font-size", "22px")
                   , ("line-height", "22px")
                   , ("padding", "0")
                   , ("user-select", "none")
                   , ("-webkit-user-select", "none")
                   ]

containerStyle :: [(String, String)]
containerStyle = [ ("background-color", "#c0c0c0")
                , ("border-top", "3px solid #ffffff")
                , ("border-left", "3px solid #ffffff")
                , ("border-right", "3px solid #7b7b7b")
                , ("border-bottom", "3px solid #7b7b7b")
                , ("padding", "10px")
                , ("display", "inline-block")
                , ("margin", "20px auto")
                ]


mineDisplayStyle :: [(String, String)]
mineDisplayStyle = timeDisplayStyle ++ 
    [ ("cursor", "pointer")
    , ("transition", "color 0.3s ease")
    ]

-- Colores de los números actualizados para coincidir exactamente
getNumberColor :: Int -> String
getNumberColor n = case n of
    1 -> "#0000ff" 
    2 -> "#008000" 
    3 -> "#ff0000" 
    4 -> "#000080" 
    5 -> "#800000" 
    6 -> "#008080" 
    7 -> "#000000"  
    8 -> "#808080"
    _ -> "#000000"

main :: IO ()
main = do
    args <- getArgs
    difficulty <- case args of
        [] -> return Easy  -- Default difficulty
        [diffStr] -> case reads diffStr of
            [(diff, "")] -> return diff
            _ -> do
                putStrLn "Invalid difficulty. Use: Easy, Medium, Hard, or Custom w h m"
                exitFailure
        [w, h, m] -> case (reads w, reads h, reads m) of
            ([(width, "")], [(height, "")], [(mines, "")]) -> 
                return $ Custom width height mines
            _ -> do
                putStrLn "Invalid custom dimensions. Use three numbers for width height mines"
                exitFailure
        _ -> do
            putStrLn "Usage: stack run [difficulty] or stack run width height mines"
            exitFailure
            
    let (width, height, mines) = difficultySettings difficulty
    startGUI defaultConfig { jsPort = Just 8023 } (setup width height mines)

setup :: Int -> Int -> Int -> Window -> UI ()
setup width height mines window = do
    game <- liftIO $ newGame width height mines
    gameRef <- liftIO $ newIORef game
    timeRef <- liftIO $ newIORef 0
    movesRef <- liftIO $ newIORef 0
    displayModeRef <- liftIO $ newIORef TimeMode
    timer <- UI.timer # set UI.interval 1000
    
    -- Disable right-click context menu
    runFunction $ ffi "document.addEventListener('contextmenu', function(e) { e.preventDefault(); });"
    
    return window # set UI.title "Minesweeper"
    body <- getBody window
    
    container <- UI.div # set UI.style containerStyle
    topPanel <- UI.div # set UI.style [("display", "flex"), ("justify-content", "space-between"), ("margin-bottom", "10px")]
    
    mineDisplay <- UI.span 
        # set UI.text (formatNumber mines) 
        # set UI.style mineDisplayStyle
    
    timeDisplay <- UI.span # set UI.text "000" # set UI.style (timeDisplayStyle ++ [("cursor", "pointer")])
    replayButton <- UI.button # set UI.text "🙂" # set UI.style replayButtonStyle
    autoPlayButton <- UI.button # set UI.text "🤖" # set UI.style replayButtonStyle

    
    grid <- UI.table # set UI.style gridStyle
    buttonsRef <- liftIO $ newIORef (Map.empty :: Map.Map Position ButtonInfo)


    -- Timer update logic
    on UI.tick timer $ \_ -> do
        currentGame <- liftIO $ readIORef gameRef
        when (gameState currentGame == Playing) $ do
            currentTime <- liftIO $ readIORef timeRef
            let newTime = currentTime + 1
            liftIO $ writeIORef timeRef newTime
            
            currentMode <- liftIO $ readIORef displayModeRef
            when (currentMode == TimeMode) $ 
                void $ element timeDisplay # set UI.text (formatNumber newTime)

    -- Time display click handler
    on UI.click timeDisplay $ const $ do
        currentMode <- liftIO $ readIORef displayModeRef
        currentGame <- liftIO $ readIORef gameRef
        case currentMode of
            TimeMode -> do
                moves <- liftIO $ readIORef movesRef
                void $ element timeDisplay # set UI.text (formatNumber moves)
                liftIO $ writeIORef displayModeRef MovesMode
            MovesMode -> do
                time <- liftIO $ readIORef timeRef
                void $ element timeDisplay # set UI.text (formatNumber time)
                liftIO $ writeIORef displayModeRef TimeMode

    -- Mine display click handler
    on UI.click mineDisplay $ const $ do
        currentGame <- liftIO $ readIORef gameRef
        when (gameState currentGame == Playing) $ do
            let remainingMines = countRemainingMines currentGame
            when (remainingMines == 0) $ do
                (newGame, positions) <- liftIO $ revealRemaining currentGame
                liftIO $ writeIORef gameRef newGame
                updateAllCells window newGame buttonsRef
                
                case gameState newGame of
                    Lost -> revealAllMines window newGame buttonsRef timer False replayButton (-1, -1)
                    Won -> revealAllMines window newGame buttonsRef timer True replayButton (-1, -1)
                    _ -> return ()

    -- Move handler
    let handleMove :: Int -> Int -> UI ()
        handleMove x y = do
            currentGame <- liftIO $ readIORef gameRef
            when (gameState currentGame /= Lost && gameState currentGame /= Won) $ do
                case getCellAt (x, y) (gameBoard currentGame) of
                    Just cell -> case (content cell, cellState cell) of
                        (Empty n, Revealed) -> do
                            -- Chord click
                            (newGame, changedPositions) <- liftIO $ handleChordClick currentGame (x, y)
                            liftIO $ writeIORef gameRef newGame
                            
                            when (newGame /= currentGame) $ do
                                moves <- liftIO $ readIORef movesRef
                                liftIO $ writeIORef movesRef (moves + 1)
                                
                                currentMode <- liftIO $ readIORef displayModeRef
                                when (currentMode == MovesMode) $
                                    void $ element timeDisplay # set UI.text (formatNumber (moves + 1))
                            
                            buttons <- liftIO $ readIORef buttonsRef
                            forM_ changedPositions $ \pos ->
                                case Map.lookup pos buttons of
                                    Just buttonInfo -> updateCell pos newGame buttonInfo
                                    Nothing -> return ()
                            
                            case gameState newGame of
                                Lost -> revealAllMines window newGame buttonsRef timer False replayButton (x, y)
                                Won -> revealAllMines window newGame buttonsRef timer True replayButton (-1, -1)
                                _ -> return ()
                        _ -> do
                            -- Regular click
                            (newGame, changedPositions) <- liftIO $ revealCell currentGame (x, y)
                            moves <- liftIO $ readIORef movesRef
                            liftIO $ writeIORef movesRef (moves + 1)
                            liftIO $ writeIORef gameRef newGame
                            
                            currentMode <- liftIO $ readIORef displayModeRef
                            when (currentMode == MovesMode) $
                                void $ element timeDisplay # set UI.text (formatNumber (moves + 1))
                            
                            buttons <- liftIO $ readIORef buttonsRef
                            forM_ changedPositions $ \pos ->
                                case Map.lookup pos buttons of
                                    Just buttonInfo -> updateCell pos newGame buttonInfo
                                    Nothing -> return ()
                            
                            case gameState newGame of
                                Lost -> revealAllMines window newGame buttonsRef timer False replayButton (x, y)
                                Won -> revealAllMines window newGame buttonsRef timer True replayButton (-1, -1)
                                _ -> return ()
                    Nothing -> return ()

    -- Grid creation
    let createGrid = do
            forM_ [0..(height-1)] $ \y -> do
                row <- UI.tr
                forM_ [0..(width-1)] $ \x -> do
                    cell <- UI.td
                    button <- UI.button # set UI.style buttonBaseStyle
                    element cell #+ [element button]
                    element row #+ [element cell]
                    
                    let buttonInfo = ButtonInfo {
                            button = button,
                            styleCache = Map.fromList [
                                ("hidden", buttonBaseStyle),
                                ("revealed", revealedStyle),
                                ("mine-win", revealedStyle),
                                ("mine-lose", clickedMineStyle),
                                ("wrong-flag", wrongFlagStyle)
                            ]
                        }
                    
                    liftIO $ modifyIORef buttonsRef $ Map.insert (x, y) buttonInfo
                    
                    on UI.click button $ const $ handleMove x y
                    on UI.contextmenu button $ const $ 
                        handleRightClick x y gameRef buttonsRef mineDisplay
                    
                element grid #+ [element row]

    -- Reset game handler
    let resetGame = do
            UI.stop timer
            element grid # set UI.children []
            
            newGameState <- liftIO $ newGame width height mines
            liftIO $ writeIORef gameRef newGameState
            liftIO $ writeIORef timeRef 0
            liftIO $ writeIORef movesRef 0
            liftIO $ writeIORef buttonsRef Map.empty
            liftIO $ writeIORef displayModeRef TimeMode
            
            void $ element timeDisplay # set UI.text "000"

            void $ element mineDisplay # set UI.text (formatNumber mines)
            updateMineDisplayColor newGameState mineDisplay

            void $ element replayButton # set UI.text "🙂"
            
            void createGrid
            UI.start timer

    on UI.click autoPlayButton $ const $ do
        currentGame <- liftIO $ readIORef gameRef
        case findBestMove currentGame of
            Just pos -> handleMove (fst pos) (snd pos)
            Nothing -> return ()  -- No move available

    -- Initial setup
    element topPanel #+ [element mineDisplay, element autoPlayButton, element replayButton, element timeDisplay]
    element container #+ [element topPanel, element grid]
    element body #+ [element container]
    void createGrid

    on UI.click replayButton $ const resetGame
    UI.start timer

-- When the game is over (win or lose), reveal all mines
revealAllMines :: Window -> Game -> IORef (Map.Map Position ButtonInfo) -> UI.Timer -> Bool -> UI.Element -> Position -> UI ()
revealAllMines window game buttonsRef timer isWin replayButton clickedPos = do
    UI.stop timer
    buttons <- liftIO $ readIORef buttonsRef
    -- Go over all cells of the game board
    forM_ (Map.toList $ gameBoard game) $ \((x, y), cell) ->
        case Map.lookup (x, y) buttons of
            Just buttonInfo -> do
                let pos = (x, y)
                case (content cell, cellState cell) of
                    -- Clicked mine
                    (Mine, _) | pos == clickedPos -> void $ 
                        element (button buttonInfo) 
                            # set UI.text "💣"
                            # set UI.style (styleCache buttonInfo Map.! "mine-lose")
                    -- Wrong flag (flagged cell without mine)
                    (Empty _, Flagged) -> void $
                        element (button buttonInfo)
                            # set UI.text "🚩"
                            # set UI.style (styleCache buttonInfo Map.! "wrong-flag")
                    -- Other mines
                    (Mine, _) -> void $ 
                        element (button buttonInfo) 
                            # set UI.text "💣"
                            # set UI.style (styleCache buttonInfo Map.! (if isWin then "mine-win" else "revealed"))
                    _ -> return ()
            Nothing -> return ()
    updateEmoji replayButton (if isWin then Won else Lost)
            

-- Update all cells of the game board
updateAllCells :: Window -> Game -> IORef (Map.Map Position ButtonInfo) -> UI ()
updateAllCells window game buttonsRef = do
    buttons <- liftIO $ readIORef buttonsRef
    forM_ (Map.toList $ gameBoard game) $ \(pos, _) ->
        case Map.lookup pos buttons of
            Just buttonInfo -> updateCell pos game buttonInfo
            Nothing -> return ()

-- Update a single cell of the game board
updateCell :: Position -> Game -> ButtonInfo -> UI ()
updateCell pos game buttonInfo = 
    case getCellAt pos (gameBoard game) of
        Just cell -> case (content cell, cellState cell) of
            (_, Hidden) -> void $ 
                element (button buttonInfo) 
                    # set UI.text ""
                    # set UI.style (styleCache buttonInfo Map.! "hidden")
            (Mine, Revealed) -> void $ 
                element (button buttonInfo) 
                    # set UI.text "💣"
                    # set UI.style (styleCache buttonInfo Map.! "mine-lose")
            (Empty 0, Revealed) -> void $
                element (button buttonInfo) 
                    # set UI.text ""
                    # set UI.style (styleCache buttonInfo Map.! "revealed")
            (Empty n, Revealed) -> void $
                element (button buttonInfo) 
                    # set UI.text (show n)
                    # set UI.style ((styleCache buttonInfo Map.! "revealed") ++ 
                                  [("color", getNumberColor n)])
            (_, Flagged) -> void $
                element (button buttonInfo) 
                    # set UI.text "🚩"
                    # set UI.style (styleCache buttonInfo Map.! "hidden")
            (_, QuestionMark) -> void $
                element (button buttonInfo) 
                    # set UI.text "❓"
                    # set UI.style (styleCache buttonInfo Map.! "hidden")
        Nothing -> return ()

-- Right-click handler, toggles between flagged, question mark and hidden states
handleRightClick :: Int -> Int -> IORef Game -> IORef (Map.Map Position ButtonInfo) -> Element -> UI Bool
handleRightClick x y gameRef buttonsRef mineDisplay = do
    currentGame <- liftIO $ readIORef gameRef
    buttons <- liftIO $ readIORef buttonsRef
    
    when (gameState currentGame == Playing) $ do
        let pos = (x, y)
            cell = getCellAt pos (gameBoard currentGame)
            buttonInfo = Map.lookup pos buttons
        
        case (cell, buttonInfo) of
            (Just currentCell, Just bInfo) -> 
                case cellState currentCell of
                    Revealed -> return ()  
                    -- If the cell is not revealed, toggle between flagged, question mark and hidden states
                    _ -> do
                        let (newGameState, _) = toggleFlag currentGame pos
                            remainingMines = countRemainingMines newGameState
                        liftIO $ writeIORef gameRef newGameState
                        updateCell pos newGameState bInfo
                        void $ element mineDisplay # set UI.text (formatNumber remainingMines)
                        updateMineDisplayColor newGameState mineDisplay
            _ -> return ()
    
    return False

-- Update the color of the mine display based on the number of remaining mines
updateMineDisplayColor :: Game -> Element -> UI ()
updateMineDisplayColor game display = do
    let remainingMines = countRemainingMines game
    void $ element display # set UI.style (timeDisplayStyle ++ 
        [("color", if remainingMines == 0 then "#0000ff" else "#ff0000")])

-- Format a number to a 3-digit string
formatNumber :: Int -> String
formatNumber n = let
    str = show n
    padded = replicate (3 - length str) '0' ++ str
    in padded

-- Update the emoji in the replay button
updateEmoji :: Element -> GameState -> UI ()
updateEmoji button state = do
    let emoji = case state of
            Won -> "😎"
            Lost -> "😵"
            Playing -> "🙂"
    void $ element button # set UI.text emoji