module MinesweeperSolver where

import Minesweeper
import qualified Data.Map.Strict as Map
import Data.List (minimumBy, find, (\\))
import Data.Ord (comparing)
import qualified Data.Set as Set

-- Main function to find the best move
findBestMove :: Game -> Maybe Position
findBestMove game
    | gameState game /= Playing = Nothing
    | otherwise = do
        let definiteMineCells = findAllDefiniteMines game Set.empty
            definitelySafeCells = findAllSafeCells game definiteMineCells
        case definitelySafeCells of
            Just pos -> Just pos
            Nothing -> findLowestRiskMove game definiteMineCells

-- Find all cells that definitely contain mines through iterative analysis
findAllDefiniteMines :: Game -> Set.Set Position -> Set.Set Position
findAllDefiniteMines game knownMines = 
    let newMines = findNewDefiniteMines game knownMines
        patternMines = findPatternMines game
        allMines = Set.union (Set.union knownMines newMines) patternMines
    in if Set.size allMines > Set.size knownMines
       -- New mines were found, so we need to reanalyze the board
       then findAllDefiniteMines game allMines
       else allMines

-- Find new definite mines based on current knowledge (revealed cells and numeric constraints)
findNewDefiniteMines :: Game -> Set.Set Position -> Set.Set Position
findNewDefiniteMines game knownMines = 
    let hiddenCells = getAllHiddenCells game
        isNewMine pos = not (Set.member pos knownMines) && 
                        (isConstrainedMine game pos knownMines)
    in Set.fromList $ filter isNewMine hiddenCells

-- Mine detection that considers number constraints
isConstrainedMine :: Game -> Position -> Set.Set Position -> Bool
isConstrainedMine game pos knownMines =
    let neighbors = getRevealedNumberedNeighbors game pos
    in any (checkNumberConstraint game pos knownMines) neighbors

-- Check if a position must be a mine based on number constraints
checkNumberConstraint :: Game -> Position -> Set.Set Position -> (Position, Int) -> Bool
checkNumberConstraint game pos knownMines (neighborPos, mineCount) =
    let adjacent = getAdjacentPositions (gameSize game) neighborPos
        hiddenCells = filter (isHidden game) adjacent
        flaggedCount = length $ filter (isFlagged game) adjacent
        knownMinesCount = length $ filter (`Set.member` knownMines) adjacent
        remainingMines = mineCount - flaggedCount - knownMinesCount
    -- If there are mines surrounding the number, 
    -- and these are as many as the remaining hidden cells, then the remaining cells are 100% mines
    in remainingMines > 0 && length hiddenCells == remainingMines

-- Find all mines based on a specific pattern
findPatternMines :: Game -> Set.Set Position
findPatternMines game =
    let revealedTwos = findRevealedTwos game
        horizontalPatterns = concat [findOneTwoOnePattern game pos True | pos <- revealedTwos]
        verticalPatterns = concat [findOneTwoOnePattern game pos False | pos <- revealedTwos]
    in Set.fromList (horizontalPatterns ++ verticalPatterns)

-- Find all revealed cells in the board that contain the number 2
findRevealedTwos :: Game -> [Position]
findRevealedTwos game =
    let revealedPositions = getRevealedCells game
        
        hasTwoMines position =
            let maybeCell = getCellAt position (gameBoard game)
            in case maybeCell of
                Nothing -> False
                Just cell -> isNumber (content cell) && getMineCount (content cell) == 2
    
    in filter hasTwoMines revealedPositions

-- Find mines based on a specific pattern (1-2-1)
findOneTwoOnePattern :: Game -> Position -> Bool -> [Position]
findOneTwoOnePattern game centerPos isHorizontal =
    let board = gameBoard game
        size = gameSize game
    
        -- Get adjacent positions (the 1s)
        (pos1, pos2) = if isHorizontal 
                    then ((fst centerPos - 1, snd centerPos), (fst centerPos + 1, snd centerPos))
                    else ((fst centerPos, snd centerPos - 1), (fst centerPos, snd centerPos + 1)) 
    
        -- Check if a position has a revealed 1
        hasRevealedOne pos =
            let maybeCell = getCellAt pos board
            in case maybeCell of
                Nothing -> False
                Just cell -> isRevealed cell && 
                            isNumber (content cell) && 
                            getMineCount (content cell) == 1

        -- Get lateral neighbors (perpendicular to pattern direction)
        getLateralNeighbors pos = if isHorizontal
            then [(fst pos - 1, snd pos), (fst pos + 1, snd pos)]
            else [(fst pos, snd pos - 1), (fst pos, snd pos + 1)]

        -- Get directional neighbors (three cells in the pattern direction)
        getDirectionalNeighbors pos direction = 
            case direction of
                "up"    -> [(fst pos - 1, snd pos - 1), (fst pos, snd pos - 1), (fst pos + 1, snd pos - 1)]
                "down"  -> [(fst pos - 1, snd pos + 1), (fst pos, snd pos + 1), (fst pos + 1, snd pos + 1)]
                "left"  -> [(fst pos - 1, snd pos - 1), (fst pos - 1, snd pos), (fst pos - 1, snd pos + 1)]
                "right" -> [(fst pos + 1, snd pos - 1), (fst pos + 1, snd pos), (fst pos + 1, snd pos + 1)]
                _       -> []

        -- Check if a position is revealed or invalid
        isRevealedOrInvalid pos = 
            not (isValidPosition size pos) || 
            not (isHidden game pos)

        -- Check if a position is hidden or invalid
        isHiddenOrInvalid pos =
            not (isValidPosition size pos) || 
            isHidden game pos

        -- Verify that lateral neighbors are revealed or invalid
        hasRevealedLaterals pos =
            all isRevealedOrInvalid (getLateralNeighbors pos)

        -- Check if pattern neighbors in one direction are all revealed or invalid
        hasRevealedNeighbors pos direction =
            all isRevealedOrInvalid (getDirectionalNeighbors pos direction)

        -- Check if pattern neighbors in one direction are all hidden or invalid
        hasHiddenNeighbors pos direction =
            all isHiddenOrInvalid (getDirectionalNeighbors pos direction)

        -- Try to find pattern with revealed cells in one direction and hidden in the opposite
        findPatternInDirections revealedDir hiddenDir =
            hasRevealedOne pos1 && 
            hasRevealedOne pos2 &&
            hasRevealedLaterals pos1 &&
            hasRevealedLaterals pos2 &&
            hasRevealedNeighbors centerPos revealedDir &&  
            hasRevealedNeighbors pos1 revealedDir &&
            hasRevealedNeighbors pos2 revealedDir &&
            hasHiddenNeighbors centerPos hiddenDir &&
            hasHiddenNeighbors pos1 hiddenDir &&
            hasHiddenNeighbors pos2 hiddenDir

        -- Get the mine positions based on the direction
        getMinePositions direction = case direction of
            "up"    -> [(fst pos1, snd pos1 - 1), (fst pos2, snd pos2 - 1)]
            "down"  -> [(fst pos1, snd pos1 + 1), (fst pos2, snd pos2 + 1)]
            "left"  -> [(fst pos1 - 1, snd pos1), (fst pos1 - 1, snd pos2)]
            "right" -> [(fst pos1 + 1, snd pos1), (fst pos1 + 1, snd pos2)]
            _       -> []

        -- Try all possible patterns
        patterns = if isHorizontal
                  then [(findPatternInDirections "down" "up", "up"),
                       (findPatternInDirections "up" "down", "down")]
                  else [(findPatternInDirections "right" "left", "left"),
                       (findPatternInDirections "left" "right", "right")]

        -- Find the first valid pattern and get its mine positions
        result = case filter fst patterns of
            (True, dir):_ -> getMinePositions dir
            _            -> []
            
    in result

-- Find all definitely safe cells through constraint analysis
findAllSafeCells :: Game -> Set.Set Position -> Maybe Position
findAllSafeCells game mineCells = 
    let hiddenCells = getAllHiddenCells game
        -- If a cell is not a mine and is definitely safe, then it can be revealed
        isSafe pos = not (Set.member pos mineCells) && 
                     isDefinitelySafe game pos mineCells
    in find isSafe hiddenCells

-- Check if a cell is definitely safe
isDefinitelySafe :: Game -> Position -> Set.Set Position -> Bool
isDefinitelySafe game pos mineCells =
    let neighbors = getRevealedNumberedNeighbors game pos
    -- If a cell is not a mine and is safe from all neighbors, then it is safe
    in any (isSafeFromNeighbor game pos mineCells) neighbors

-- Check if a position is safe based on a neighbor's constraints
isSafeFromNeighbor :: Game -> Position -> Set.Set Position -> (Position, Int) -> Bool
isSafeFromNeighbor game pos mineCells (neighborPos, mineCount) =
    let adjacent = getAdjacentPositions (gameSize game) neighborPos
        flaggedCount = length $ filter (isFlagged game) adjacent
        knownMinesCount = length $ filter (`Set.member` mineCells) adjacent
        totalMines = flaggedCount + knownMinesCount
    -- If the number of mines surrounding the neighbor is equal to the number of mines it should have, then the remaining cells are safe
    in totalMines == mineCount && not (Set.member pos mineCells)

-- Enhanced risk calculation for remaining moves
findLowestRiskMove :: Game -> Set.Set Position -> Maybe Position
findLowestRiskMove game knownMines =
    let hiddenCells = filter (`Set.notMember` knownMines) (getAllHiddenCells game)
        riskScores = [(pos, calculateEnhancedRisk game pos knownMines) | pos <- hiddenCells]
    in case riskScores of
        [] -> Nothing
        -- Find the cell with the lowest risk 
        scores -> Just $ fst $ minimumBy (comparing snd) scores

-- Calculate enhanced risk score considering all available information
calculateEnhancedRisk :: Game -> Position -> Set.Set Position -> Double
calculateEnhancedRisk game pos knownMines =
    let neighbors = getRevealedNumberedNeighbors game pos
        neighborRisks = map (calculateNeighborRisk game pos knownMines) neighbors
        defaultRisk = defaultRiskCalculation game knownMines
    in if null neighborRisks 
       then defaultRisk 
       else maximum (defaultRisk : neighborRisks)

-- Calculate risk from a specific neighbor
calculateNeighborRisk :: Game -> Position -> Set.Set Position -> (Position, Int) -> Double
calculateNeighborRisk game pos knownMines (neighborPos, mineCount) =
    let adjacent = getAdjacentPositions (gameSize game) neighborPos
        flaggedCount = length $ filter (isFlagged game) adjacent
        -- Known mines that are adjacent to the neighbor
        knownMinesHere = length $ filter (`Set.member` knownMines) adjacent
        hiddenCells = filter (isHidden game) adjacent
        remainingMines = mineCount - flaggedCount - knownMinesHere
        remainingHidden = length hiddenCells
    in if remainingHidden == 0 
       then 0.0 
       else fromIntegral remainingMines / fromIntegral remainingHidden

-- Default risk calculation, based on the number of remaining mines and hidden cells
defaultRiskCalculation :: Game -> Set.Set Position -> Double
defaultRiskCalculation game knownMines =
    let remainingMines = mineCount game - Set.size knownMines - 
                        length (filter (isFlagged game) (getAllHiddenCells game))
        -- Total hidden cells that are not known to be mines
        totalHidden = length $ filter (`Set.notMember` knownMines) (getAllHiddenCells game)
    in if totalHidden == 0 
       then 1.0 
       else fromIntegral remainingMines / fromIntegral totalHidden

-- Get the neighbors of a position that are revealed and numbered
getRevealedNumberedNeighbors :: Game -> Position -> [(Position, Int)]
getRevealedNumberedNeighbors game pos =
    let neighbors = getAdjacentPositions (gameSize game) pos
        isNumberedCell p = case getCellAt p (gameBoard game) of
            Just cell -> isRevealed cell && isNumber (content cell)
            Nothing -> False
        getNumber p = case getCellAt p (gameBoard game) of
            Just cell -> getMineCount (content cell)
            Nothing -> 0
    in [(p, getNumber p) | p <- neighbors, isNumberedCell p]

-- Helper funciton to debug pattern detection
debugFindPatterns :: Game -> Position -> ([Position], [Position])
debugFindPatterns game pos = 
    (findOneTwoOnePattern game pos True,
     findOneTwoOnePattern game pos False)