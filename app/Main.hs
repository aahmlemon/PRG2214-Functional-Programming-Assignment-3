{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# Language LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use catMaybes" #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as L
import System.IO(hFlush, stdout)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Directory (doesFileExist, removeFile)
import Data.Maybe (mapMaybe)
import System.Exit (exitSuccess)
import Text.Tabular
    ( Header(Header, Group), Properties(SingleLine, NoLine), Table(Table) )
import Text.Tabular.AsciiArt (render)


-- Type Classes
-- To render a character
class Renderable a where
  render :: a -> Char

-- To move an entity
class Movable a where
  move :: GameBoard -> a -> a

-- For entity collision
class Collidable a where
  collidesWith :: a -> Position -> Bool

-- The position of an entity
class (Renderable a, Movable a, Collidable a) => Entity a where
  entityPos :: a -> Position

-- Player
newtype Player = Player Position deriving (Show)
-- Player can be rendered as @
instance Renderable Player where
  render :: Player -> Char
  render _ = '@'
-- Player can move on the board
instance Movable Player where
  move :: GameBoard -> Player -> Player
  move _ (Player pos) = Player pos
-- Player can collide with walls and enemies
instance Collidable Player where
  collidesWith :: Player -> Position -> Bool
  collidesWith (Player pos) targetPos = pos == targetPos
-- Player is an entity itself
instance Entity Player where
  entityPos :: Player -> Position
  entityPos (Player pos) = pos

-- Types for the board
type Position = (Int, Int) -- Coordinates of row column
type GameBoard = [T.Text] -- Is a list of Text

-- Direction for Enemies (Important since they only move on an axis)
data Direction = Downward | Upward | Leftward | Rightward deriving (Eq, Show)
-- Enemy position and direction
data Enemy = Enemy
  { enemyPos :: Position
  , enemyDir :: Direction
  } deriving (Show)
-- All enemies that are renderable (V, ^, <, >) Should have made into a small v honestly
instance Renderable Enemy where
  render :: Enemy -> Char
  render (Enemy _ Downward) = 'V'
  render (Enemy _ Upward) = '^'
  render (Enemy _ Leftward) = '<'
  render (Enemy _ Rightward) = '>'
-- Enemy can move along an axis
-- Row is represented as x, column is represented as y
instance Movable Enemy where
  move :: GameBoard -> Enemy -> Enemy
  move b (Enemy (x, y) Downward) -- If enemy is facing downwards, check to see if the next row down is a wall, if not, move down, otherwise, turn upward
    | x + 1 < length b && T.index (b !! (x + 1)) y /= '#' = Enemy (x + 1, y) Downward
    | otherwise = Enemy (x, y) Upward
  move b (Enemy (x, y) Upward) -- If enemy is facing upward, check to see if the next row up is a wall, if not, move up, otherwise, turn downward
    | x - 1 >= 0 && T.index (b !! (x - 1)) y /= '#' = Enemy (x - 1, y) Upward
    | otherwise = Enemy (x, y) Downward 
  move b (Enemy (x, y) Leftward) -- If enemy is facing leftward, check to see if the next left column is a wall, if not, move left, otherwise, turn right
    | y - 1 >= 0 && T.index (b !! x) (y - 1) /= '#' = Enemy (x, y - 1) Leftward
    | otherwise = Enemy (x, y) Rightward
  move b (Enemy (x, y) Rightward) -- If enemy is facing rightward, check to see if the next right column is a wall, if not, move right, otherwise, turn left
    | y + 1 < T.length (b !! x) && T.index (b !! x) (y + 1) /= '#' = Enemy (x, y + 1) Rightward
    | otherwise = Enemy (x, y) Leftward
-- Enemy collision
instance Collidable Enemy where
  collidesWith :: Enemy -> Position -> Bool
  collidesWith (Enemy pos _) targetPos = pos == targetPos
-- Enemy is an instance of an entity
instance Entity Enemy where
  entityPos :: Enemy -> Position
  entityPos (Enemy pos _) = pos
-- Game stats that shows total moves, enemies defeated, and bombs used (Should have added score in this as well)
data GameStats = GameStats
  { totalMoves     :: Int
  , enemiesDefeated :: Int
  , bombsUsed      :: Int
  } deriving (Show)
-- Semigroup to form into a monoid, for ease of calculation
instance Semigroup GameStats where
  (<>) :: GameStats -> GameStats -> GameStats
  GameStats moves1 defeated1 bombs1 <> GameStats moves2 defeated2 bombs2 =
    GameStats (moves1 + moves2) (defeated1 + defeated2) (bombs1 + bombs2)
-- Monoid for empty stats and to append two GameStats together
instance Monoid GameStats where
  mempty :: GameStats
  mempty = GameStats 0 0 0
  mappend :: GameStats -> GameStats -> GameStats
  mappend = (<>)

data GameState = GameState
  { maps :: [GameBoard]      -- List of all maps
  , currentMapIndex :: Int   -- Index of the current map
  , baseBoard :: GameBoard   -- Current map's base board
  , player :: Player         -- Player information
  , enemies :: [Enemy]       -- List of enemies on the current map
  , score :: Int             -- Player's score
  , bombs :: Int             -- Number of bombs the player has
  , stats :: GameStats       -- Game stats for total moves, total enemies killed, and total bombs used
  }
-- Stats Error shows an error, while StatsValue will wrap the value in a
data MonadStats a = StatsError String | StatsValue a deriving (Show, Eq)
-- Allows applying a function to MonadStats
instance Functor MonadStats where
  fmap :: (a -> b) -> MonadStats a -> MonadStats b
  fmap _ (StatsError msg) = StatsError msg
  fmap f (StatsValue x)   = StatsValue (f x)
-- Allows chaining with <*>
instance Applicative MonadStats where
  pure :: a -> MonadStats a
  pure = StatsValue
  (<*>) :: MonadStats (a -> b) -> MonadStats a -> MonadStats b
  StatsError msg <*> _ = StatsError msg
  _ <*> StatsError msg = StatsError msg
  StatsValue f <*> StatsValue x = StatsValue (f x)
-- Allows sequencing operations (>>=)
instance Monad MonadStats where
  return :: a -> MonadStats a
  return = pure
  (>>=) :: MonadStats a -> (a -> MonadStats b) -> MonadStats b
  StatsError msg >>= _ = StatsError msg
  StatsValue x >>= f   = f x

-- Headers for the table for score
csvHeaders :: [String]
csvHeaders = ["Final Score", "Total Moves", "Enemies Defeated", "Bombs Used"]

-- Overlays the entites inside the board/map
overlayEntities :: (Entity a) => GameBoard -> [a] -> GameBoard
overlayEntities = foldr (\entity board -> replace2D (entityPos entity) (Main.render entity) board)

-- Replace a character in the board (When it is taken removed)
replace2D :: Position -> Char -> GameBoard -> GameBoard
replace2D (r, c) ch b =
  let row' = b !! r
      newRow = T.take c row' `T.append` T.singleton ch `T.append` T.drop (c + 1) row'
  in take r b ++ [newRow] ++ drop (r + 1) b

-- Renders the board
renderBoard :: GameState -> IO ()
renderBoard (GameState _ _ base (Player playerPos) enemies' score' bombs' _) =
  let baseWithSpaces = map (T.map (\c -> if c == 'X' then ' ' else c)) base -- Makes X's in the maps into a square space (X was used for ease of making the maps)
      boardWithPlayer = replace2D playerPos '@' baseWithSpaces -- Overlays the board with the player (@)
      finalBoard = overlayEntities boardWithPlayer enemies' -- Overlays the board with enemies
  in clearScreen >>
     setCursorPosition 0 0 >>
     TIO.putStrLn (T.unlines finalBoard) >>
     TIO.putStrLn (T.pack ("Current Score: " ++ show score')) >>
     TIO.putStrLn (T.pack ("Number of Bombs: " ++ show bombs'))

-- Checks for valid moves
validMoves :: GameState -> [(Int, String, Position)]
validMoves state@(GameState _ _ _ (Player (pr, pc)) _ _ bombs' _) =
  let basicMoves =
        [ (1, "Move Up", (pr - 1, pc)) -- The board starts from top left so it checks row above it
        , (2, "Move Right", (pr, pc + 1)) -- Checks the column to the right
        , (3, "Move Down", (pr + 1, pc)) -- Checks row below
        , (4, "Move Left", (pr, pc - 1)) -- Checks column to the left
        , (5, "Wait", (pr, pc)) -- Just waits in place
        ]
      bombMoves =
        [ (6, "Bomb Up", (pr - 1, pc)) -- Same as above, but to see if there is an enemy there or not
        , (7, "Bomb Right", (pr, pc + 1))
        , (8, "Bomb Down", (pr + 1, pc))
        , (9, "Bomb Left", (pr, pc - 1))
        ]
      validBasicMoves = filter (\(_, _, pos) -> isValidMove pos state) basicMoves -- If there is a wall or enemy right next, then you can't move there
      validBombMoves = if bombs' > 0
                       then filter (\(_, _, pos) -> isValidBomb pos state) bombMoves -- Checks validity for where to throw the bombs
                       else [] -- If no bombs, no need to check validity 
  in validBasicMoves ++ validBombMoves -- combines both validity checks

-- Checks to see if a tile can be moved to or not
isValidMove :: Position -> GameState -> Bool
isValidMove pos (GameState _ _ b _ enemies' _ _ _) =
  let withinBounds = pos `elem` [(r, c) | r <- [0 .. length b - 1], c <- [0 .. T.length (head b) - 1]]
      notWall = withinBounds && T.index (b !! fst pos) (snd pos) /= '#'
      notEnemy = all (\enemy -> entityPos enemy /= pos) enemies'
  in withinBounds && notWall && notEnemy

-- Checks to see if there is any enemy around the player for bombing specifically
isValidBomb :: Position -> GameState -> Bool
isValidBomb pos (GameState _ _ _ _ enemies' _ _ _) =
  any (\enemy -> entityPos enemy == pos) enemies'

-- The actual action of the bombing
bombAttack :: Int -> GameState -> GameState
bombAttack direction state@(GameState _ _ b (Player (pr, pc)) enemies' score' bombs' stats') =
  let targetPos = case direction of
                    6 -> (pr - 1, pc) -- Bomb Up
                    7 -> (pr, pc + 1) -- Bomb Right
                    8 -> (pr + 1, pc) -- Bomb Down
                    9 -> (pr, pc - 1) -- Bomb Left
                    _ -> (pr, pc)
      (remainingEnemies, hitEnemies) = L.partition (\e -> entityPos e /= targetPos) enemies' -- Checks for all enemies on the tile and removes them from the board
      numBombedEnemies = length hitEnemies
      newStats = stats' <> GameStats { totalMoves = 0, enemiesDefeated = numBombedEnemies, bombsUsed = 1 } -- Append is used here to easily increase the value
  in if numBombedEnemies > 0 -- If an enemy was bombed
        then state { enemies = remainingEnemies
                   , score = score' + numBombedEnemies
                   , bombs = bombs' - 1
                   , baseBoard = replace2D targetPos ' ' b
                   , stats = newStats }
        else state { stats = stats' <> GameStats { totalMoves = 0, enemiesDefeated = 0, bombsUsed = 1 } }

-- Collision Check
checkCollision :: (Collidable a) => Position -> [a] -> Bool
checkCollision pos = any (`collidesWith` pos)

-- Function that moves the player
movePlayer :: Int -> GameState -> GameState
movePlayer choice state@(GameState maps' idx b (Player _) _ score' bombs' stats') =
  case newPos of
    Just pos ->
      let newStats = stats' <> GameStats { totalMoves = 1, enemiesDefeated = 0, bombsUsed = 0 } -- Increments totalMoves everytime an action is taken
      in if isTransitionTile
            then if idx + 1 < length maps' -- Checks the number of maps in the array
                 then transitionToNextMap state { stats = newStats }
                 else state { stats = newStats }
         else state { player = Player pos, baseBoard = updatedBoard, score = updatedScore, bombs = updatedBombs, stats = newStats }
    Nothing -> state
  where
    moves = validMoves state
    newPos = lookup choice [(num, pos) | (num, _, pos) <- moves]
    isTransitionTile = maybe False (\pos -> T.index (b !! fst pos) (snd pos) == 'O') newPos -- Checks for transition tile
    isScoreTile = maybe False (\pos -> T.index (b !! fst pos) (snd pos) == 'S') newPos -- Checks if it is a score tile
    isBombTile = maybe False (\pos -> T.index (b !! fst pos) (snd pos) == 'B') newPos -- Checks if it is a bomb tile

    updatedScore = if isScoreTile then score' + 1 else score' -- Updates
    updatedBombs = if isBombTile then bombs' + 1 else bombs'
    updatedBoard = case newPos of
                     Just pos -> if isScoreTile || isBombTile -- Pick up the bomb/score and removes it
                                 then replace2D pos ' ' b
                                 else b
                     Nothing -> b

-- Function to help move to the next map
transitionToNextMap :: GameState -> GameState
transitionToNextMap state@(GameState maps' idx _ _ _ _ _ _) =
  if nextIndex < length maps' -- As long as it's within the length of the array
    then state { currentMapIndex = nextIndex
               , baseBoard = cleanedMap
               , player = Player playerStartPos
               , enemies = enemiesInNextMap
               }
    else state -- No more maps, stay on the current map
  where
    nextIndex = idx + 1
    nextMap = maps' !! nextIndex
    playerStartPos = findFirstPosition '@' nextMap
    enemiesInNextMap = parseEnemies nextMap
    cleanedMap = T.map (\c -> if T.any (== c) "@V^<>" then 'X' else c) <$> nextMap

-- Moves enemies according to the direction they are facing
moveEnemies :: GameState -> GameState
moveEnemies state =
  state { enemies = map (move (baseBoard state)) (enemies state) }

-- Initialises the maps in that are loaded in main
initializeGameState :: [GameBoard] -> GameState
initializeGameState maps' = GameState
  { maps = maps'
  , currentMapIndex = 0
  , baseBoard = cleanedMap
  , player = Player playerStartPos
  , enemies = parseEnemies firstMap
  , score = 0
  , bombs = 0
  , stats = mempty
  }
  where
    firstMap = head maps'
    cleanedMap = T.map (\c -> if T.any (== c) (T.pack "@V^<>") then 'X' else c) <$> firstMap
    playerStartPos = findFirstPosition '@' firstMap

-- Checks to see if the textfile contains a valid enclosed map
isValidMap :: GameBoard -> Bool
isValidMap board =
  let rowLengths = map T.length board
      topAndBottomValid = T.all (== '#') (head board) && T.all (== '#') (last board)
      sideWallsValid = all (\row' -> T.head row' == '#' && T.last row' == '#') (init (tail board))
      uniformWidth = all (== head rowLengths) rowLengths
      hasOnePlayer = length (findAllPositions '@' board) == 1
  in not (null board) && uniformWidth && topAndBottomValid && sideWallsValid && hasOnePlayer

-- Parses all the boards
parseAllBoards :: [T.Text] -> [GameBoard]
parseAllBoards contents =
  let boards = map T.lines contents
  in if all isValidMap boards
        then boards
        else error "One or more maps are invalid. Please ensure all maps are valid."

-- Parses the enemies from the text file
parseEnemies :: GameBoard -> [Enemy]
parseEnemies board =
  let downwardEnemies = map (`Enemy` Downward) (findAllPositions 'V' board)
      upwardEnemies   = map (`Enemy` Upward) (findAllPositions '^' board)
      leftwardEnemies = map (`Enemy` Leftward) (findAllPositions '<' board)
      rightwardEnemies = map (`Enemy` Rightward) (findAllPositions '>' board)
  in downwardEnemies ++ upwardEnemies ++ leftwardEnemies ++ rightwardEnemies

-- Find all positions of a specific character in the board
findAllPositions :: Char -> GameBoard -> [Position]
findAllPositions ch board =
  [(r, c) | (r, row') <- zip [0..] board, (c, cell) <- zip [0..] (T.unpack row'), cell == ch]

-- Find the first position of a specific character in the board (Used for player)
findFirstPosition :: Char -> GameBoard -> Position
findFirstPosition ch board =
  case findAllPositions ch board of
    (pos:_) -> pos
    []      -> error ("Character " ++ show ch ++ " not found on the board!")

-- The core "loop" of the game
gameLoop :: GameState -> IO ()
gameLoop state@(GameState _ _ b (Player playerPos) _ score' _ stats')
  | T.index (b !! fst playerPos) (snd playerPos) == 'E' =
      let filePath = "app/game_stats.csv" -- Output file
      in TIO.putStrLn "Congratulations! You've reached the end of the game!" >>
         displayGameStats stats' score' >>
         writeStatsToCsv score' stats' filePath >>
         TIO.putStrLn "Press Enter to go back to the main menu." >>
         getLine >> mainMenu
  | otherwise =
      renderBoard state >>
      TIO.putStrLn (T.unlines [T.pack ("[" ++ show num ++ "] " ++ desc) | (num, desc, _) <- validMoves state]) >>
      TIO.putStr "Choose your move: " >>
      hFlush stdout >>
      getLine >>= \input ->
        let maybeChoice = reads input :: [(Int, String)]
        in case maybeChoice of
             [(choice, "")] ->
               let validChoices = [num | (num, _, _) <- validMoves state]
               in if choice `elem` validChoices
                    then if choice `elem` [6, 7, 8, 9] -- Bomb-related choices
                         then gameLoop (bombAttack choice state)
                         else let newState = moveEnemies (movePlayer choice state)
                              in if checkCollision (entityPos (player newState)) (enemies newState)
                                   then gameOver newState
                                   else gameLoop newState
                    else invalidInput
             _ -> invalidInput
  where
    gameOver newState =
      let filePath = "app/game_stats.csv" -- Output file
      in TIO.putStrLn "Game Over! The enemies got you!" >>
         displayGameStats (stats newState) (score newState) >>
         writeStatsToCsv (score newState) (stats newState) filePath >>
         TIO.putStrLn "Press Enter to go back to the main menu." >>
         getLine >> mainMenu

    invalidInput =
      TIO.putStrLn "Invalid Input, please enter a correct command." >> gameLoop state

-- Helper function for gameLoop to display the end stats
displayGameStats :: GameStats -> Int -> IO ()
displayGameStats stats' finalScore' =
    TIO.putStrLn "Game Stats Summary:" >>
    TIO.putStrLn (T.pack $ "Final Score: " ++ show finalScore') >>
    TIO.putStrLn (T.pack $ "Total Moves: " ++ show (totalMoves stats')) >>
    TIO.putStrLn (T.pack $ "Enemies Defeated: " ++ show (enemiesDefeated stats')) >>
    TIO.putStrLn (T.pack $ "Bombs Used: " ++ show (bombsUsed stats'))

-- Writes the scores into a CSV
writeStatsToCsv :: Int -> GameStats -> FilePath -> IO ()
writeStatsToCsv finalScore' stats' filePath =
    doesFileExist filePath >>= \fileExists ->
        let header = T.intercalate "," ["Final Score", "Total Moves", "Enemies Defeated", "Bombs Used"]
            row' = T.intercalate ","
                [ T.pack (show finalScore')
                , T.pack (show (totalMoves stats'))
                , T.pack (show (enemiesDefeated stats'))
                , T.pack (show (bombsUsed stats'))
                ]
            newContent =
                if fileExists
                    then T.unlines [row']
                    else T.unlines [header, row']
        in if fileExists
            then TIO.appendFile filePath newContent
            else TIO.writeFile filePath newContent

-- Helper function to parse a single CSV row
parseCsvRow :: T.Text -> Maybe [Int]
parseCsvRow =
    traverse (readMaybe . T.unpack . T.strip) . T.splitOn ","

-- ReadMaybe function for safe conversion
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

-- Compute the sum of a column
sumColumn :: [[Int]] -> Int -> MonadStats Int
sumColumn rows colIndex =
    if all (\row' -> length row' > colIndex) rows
       then StatsValue $ sum (map (!! colIndex) rows)
       else StatsError "Invalid column index."

-- Parse CSV file into rows of integers, skipping the header
parseCsv :: FilePath -> IO (MonadStats [[Int]])
parseCsv filePath =
    TIO.readFile filePath >>= \contents ->
        let rows = map parseCsvRow (drop 1 $ T.lines contents) -- Skip header
        in if Nothing `elem` rows
              then return $ StatsError $ "Failed to parse CSV rows: " ++ show (filter (== Nothing) rows)
              else return $ StatsValue $ mapMaybe id rows

-- Compute both sum and average for a specific column
computeColumnStats :: [[Int]] -> Int -> MonadStats (Int, Double)
computeColumnStats rows colIndex =
    let sumStats = sumColumn rows colIndex
        count = length rows
    in case sumStats of
        StatsError msg -> StatsError msg
        StatsValue total -> StatsValue (total, fromIntegral total / fromIntegral count)

-- Computes the sum and average of the stats in the csv file
computeCsvStats :: FilePath -> IO (MonadStats (Int, Double, Int, Double, Int, Double, Int, Double))
computeCsvStats filePath =
    parseCsv filePath >>= \case
      StatsError msg -> return $ StatsError msg
      StatsValue rows ->
          let scoreStats  = computeColumnStats rows 0  -- Final Score column
              movesStats  = computeColumnStats rows 1  -- Total Moves column
              enemiesStats = computeColumnStats rows 2 -- Enemies Defeated column
              bombsStats  = computeColumnStats rows 3  -- Bombs Used column
          in return $ case (scoreStats, movesStats, enemiesStats, bombsStats) of
              (StatsError msg, _, _, _) -> StatsError msg -- Pattern matching to see if there is any error
              (_, StatsError msg, _, _) -> StatsError msg
              (_, _, StatsError msg, _) -> StatsError msg
              (_, _, _, StatsError msg) -> StatsError msg
              (StatsValue (sumScore, avgScore), -- Combine all into one
               StatsValue (sumMoves, avgMoves),
               StatsValue (sumEnemies, avgEnemies),
               StatsValue (sumBombs, avgBombs)) ->
                  StatsValue (sumScore, avgScore, sumMoves, avgMoves, sumEnemies, avgEnemies, sumBombs, avgBombs)

-- Main menu options
mainMenu :: IO ()
mainMenu =
    clearScreen >>
    setCursorPosition 0 0 >>
    TIO.putStrLn (T.unlines ["Welcome to the Tile Adventure Game!", "Main Menu:", "1. Play Game", "2. View Scores", "3. Exit"]) >>
    TIO.putStr "Enter your choice: " >>
    hFlush stdout >> -- Removes the buffer in stdout (standard out)
    getLine >>= handleMainMenu

-- Handles the main menu options, pattern matched to just find the valid options
handleMainMenu :: String -> IO ()
handleMainMenu "1" = startGame
handleMainMenu "2" = scoreMenu
handleMainMenu "3" = TIO.putStrLn "Goodbye! Have a nice day." >> exitSuccess -- Exits the game successfully
handleMainMenu _   = TIO.putStrLn "Invalid choice. Please try again." >> mainMenu -- returns to main menu

-- Starts the game (Only called if player plays game)
startGame :: IO ()
startGame =
    TIO.readFile "app/map1.txt" >>= \map1 ->
    TIO.readFile "app/map2.txt" >>= \map2 ->
    TIO.readFile "app/map3.txt" >>= \map3 ->
    TIO.readFile "app/map4.txt" >>= \map4 ->
    let maps' = parseAllBoards [map1, map2, map3, map4]
    in gameLoop (initializeGameState maps') >> mainMenu

-- Score submenu to display the score options
scoreMenu :: IO ()
scoreMenu =
    clearScreen >>
    setCursorPosition 0 0 >>
    TIO.putStrLn (T.unlines
        [ "Score Menu:"
        , "1. View All Stats"
        , "2. Show Sum and Average Stats"
        , "3. Delete all scores"
        , "4. Return to main menu"
        ]) >>
    TIO.putStr "Enter your choice: " >>
    hFlush stdout >>
    getLine >>= handleScoreMenu

-- Handles the score commands
handleScoreMenu :: String -> IO ()
handleScoreMenu "1" =
    doesFileExist "app/game_stats.csv" >>= \exists ->
        if exists
            then parseCsv "app/game_stats.csv" >>= displayAllStats >>
                 TIO.putStrLn "Press Enter to return." >> getLine >> scoreMenu
            else TIO.putStrLn "No score file found. Please play the game to generate stats." >>
                 TIO.putStrLn "Press Enter to return." >> getLine >> scoreMenu

handleScoreMenu "2" =
    doesFileExist "app/game_stats.csv" >>= \exists ->
        if exists
            then computeCsvStats "app/game_stats.csv" >>= displayAvgSumStats >>
                 TIO.putStrLn "Press Enter to return." >> getLine >> scoreMenu
            else TIO.putStrLn "No score file found. Please play the game to generate stats." >>
                 TIO.putStrLn "Press Enter to return." >> getLine >> scoreMenu

handleScoreMenu "3" =
    doesFileExist "app/game_stats.csv" >>= \exists ->
        if exists
            then TIO.putStrLn "Are you sure you want to delete the score file? (y/n)" >>
                 getLine >>= \response ->
                     if response `elem` ["y", "Y"]
                         then removeFile "app/game_stats.csv" >>
                              TIO.putStrLn "Score file deleted." >>
                              TIO.putStrLn "Press Enter to return." >> getLine >> scoreMenu
                         else TIO.putStrLn "Deletion cancelled." >>
                              TIO.putStrLn "Press Enter to return." >> getLine >> scoreMenu
            else TIO.putStrLn "No score file found to delete." >>
                 TIO.putStrLn "Press Enter to return." >> getLine >> scoreMenu
                
handleScoreMenu "4" = mainMenu

handleScoreMenu _ =
    TIO.putStrLn "Invalid choice. Please try again." >> scoreMenu

displayAllStats :: MonadStats [[Int]] -> IO ()
displayAllStats (StatsValue rows) =
    if null rows
        then TIO.putStrLn "No data to display." -- Handles empty rows
        else do
            let table = Table
                          (Group NoLine (map (Header . show) [1..length rows])) -- Row indices
                          (Group SingleLine (map Header csvHeaders))           -- Column headers
                          (map (map show) rows)                               -- Convert data to strings
            putStrLn $ Text.Tabular.AsciiArt.render id id id table            -- Renders table
displayAllStats (StatsError msg) =
    TIO.putStrLn (T.pack $ "Error: " ++ msg)

-- Displays Sum and Average of stats
displayAvgSumStats :: MonadStats (Int, Double, Int, Double, Int, Double, Int, Double) -> IO ()
displayAvgSumStats (StatsValue (sumScore, avgScore, sumMoves, avgMoves, sumEnemies, avgEnemies, sumBombs, avgBombs)) =
    TIO.putStrLn "Stats Summary:" *>
    TIO.putStrLn (T.pack $ "Total Score: " ++ show sumScore ++ ", Average Score: " ++ show avgScore) *>
    TIO.putStrLn (T.pack $ "Total Moves: " ++ show sumMoves ++ ", Average Moves: " ++ show avgMoves) *>
    TIO.putStrLn (T.pack $ "Total Enemies Defeated: " ++ show sumEnemies ++ ", Average Enemies Defeated: " ++ show avgEnemies) *>
    TIO.putStrLn (T.pack $ "Total Bombs Used: " ++ show sumBombs ++ ", Average Bombs Used: " ++ show avgBombs)
displayAvgSumStats (StatsError msg) =
    TIO.putStrLn (T.pack $ "Error: " ++ msg)

-- Main function
main :: IO ()
main = mainMenu
