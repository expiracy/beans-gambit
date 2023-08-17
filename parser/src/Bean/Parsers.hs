{- Module contains all the parsers -}
module Bean.Parsers where

import Bean.Types
import Bean.Game

import Text.Megaparsec 
import Text.Megaparsec.Char

import Control.Applicative.Combinators() 
import Control.Monad

import Data.Void hiding (show)
import Data.Char (isDigit, isLetter, ord, isSpace)
import Data.Maybe (fromJust, isNothing, isJust)

{- Parses a string value -}
type Parser = Parsec Void String

{- Parses the game file string by calling the recursive function parseTurn with the initial state -}
parseGame :: String -> Either (ParseErrorBundle String Void) [GameState]
parseGame game = do 
    -- Identify the first player to move in the game
    startPlayer <- parse parseStartPlayer "" game

    -- Defining the initial game state
    let initialGameState = GameState {currentTurnNumber = 1, currentBoard = startingPos, currentPlayer = Just startPlayer, needsUserInput = False}
    parse (parseTurn [initialGameState]) "" game

{- Get the player that is first to move in the game -}
parseStartPlayer :: Parser Player
parseStartPlayer = do
    _ <- parseTurnNumber 1
    _ <- satisfy isSpace
    coord <- parseBGNCoord

    -- Check that the player is valid
    case getPlayerFromCoord startingPos coord of 
        Nothing -> fail "Could not determine the player that is first to move in the game as an empty square was moved."
        Just player -> pure player

{- Parses the expected turn number and the appropriate turn actions -}
parseTurn :: [GameState] -> Parser [GameState]
parseTurn (gameState:gameStates) = do
    -- Parses and validates the line (turn) number
    expectedTurnNumber <- parseTurnNumber turnNumber

    -- Parses the COMPULSORY first turn action
    newGameState <- parseTurnAction (gameState:gameStates)

    if isNothing (currentPlayer newGameState) || needsUserInput newGameState
        then pure (newGameState:gameStates) -- replace the head with the new game state
        else parseSecondTurnAction newGameState -- parsing the second turn action only if game ending is not encountered
    
    where
        -- Frequently used value
        turnNumber = currentTurnNumber gameState

        -- Parses the second action in a turn
        parseSecondTurnAction newGameState = do     
            newGameState' <- parseTurnAction (newGameState:gameState:gameStates)

            -- Check if the game has been declared as ended
            if isNothing (currentPlayer newGameState') || needsUserInput newGameState'
                then pure (newGameState':gameState:gameStates) -- replace the head with the new game state
                else parseNextTurn (newGameState':newGameState:gameState:gameStates) -- parse next term as end status not reached
        
        -- Expects next turn, updates turn number and game state for next turn
        parseNextTurn (newGameState:gameStates) = do 
            _ <- many $ char ' ' -- Ignore any trailing spaces on line
            _ <- eol 
            let nextGameState = newGameState {currentTurnNumber = succ turnNumber} -- Increments expected turn counter
            parseTurn (nextGameState:gameStates) --  and parse next turn

{- A turn action is a draw, a loss or a move -}
parseTurnAction :: [GameState] -> Parser GameState
parseTurnAction gameStates = do 
    _ <- satisfy isSpace
    parseGameEnd gameStates <|> parseBGNMove gameStates 

{- The line number in the BGN file is the turn number -}
parseTurnNumber :: Int -> Parser Int
parseTurnNumber turnNumber = do
    parsedTurnNumber <- some (satisfy isDigit) <?> "digit (turn/line number)"
    _ <- string "."
    
    -- Check that the parsed turn number matches the expected turn number
    if read parsedTurnNumber == turnNumber 
        then pure $ read parsedTurnNumber
        else fail ("Incorrect turn number declared. Expected: " ++ show turnNumber ++ " but got: " ++ parsedTurnNumber)

{- Returns the boards and the expected next player -}
parseBGNMove :: [GameState] -> Parser GameState
parseBGNMove gameStates = do
    let currentGameState = head gameStates 
    let player = currentPlayer currentGameState
    let (board:boards) = map currentBoard gameStates

    -- Checking that the game is not drawn or lost before making any move
    when (gameIsDrawn (board:boards))
        $ fail ("Encountered draw that was not declared in the BGN file.\n" ++ show gameStates)
    
    when (gameIsWon board (fromJust player))
        $ fail ("Encountered loss that was not declared in the BGN file.\n" ++ show currentGameState)

    -- Parse the move
    startCoord <- parseBGNCoord 
    takeSymbol <- optional $ char 'x'
    endCoord <- parseBGNCoord

    -- Ensure expected player is moving the piece
    when (player /= getPlayerFromCoord board startCoord)
        $ fail ("Incorrect player moved piece or moved piece was empty.\nExpected player to move was " ++ show player ++ 
                "\nCoordinate of moved piece was " ++ show startCoord ++ "\n" ++
                show currentGameState) 
    
    -- Validate the move
    let newBoard = makeMove board startCoord endCoord
    when (isNothing newBoard) 
        $ fail ("Specified move is invalid: " ++ show startCoord ++ " to " ++ show endCoord ++ "\n" ++ show currentGameState)

    -- Validate the take declaration
    let balChange = balance (fromJust newBoard) - balance board
    unless (isNothing takeSymbol && balChange == 0 || takeSymbol == Just 'x' && abs balChange == 1)
        $ fail ("Take declaration for move is incorrectly declared.\nMove was: " 
                ++ show startCoord ++ " to " ++ show endCoord ++ "\n" ++ show currentGameState)
    
    -- Values for next action
    pure $ currentGameState {currentBoard = fromJust newBoard, currentPlayer = opponent <$> player}

{- Parses and validates the declaration for the end of the game -}
parseGameEnd :: [GameState] -> Parser GameState
parseGameEnd gameStates = do
    -- Frequently used values
    let currentGameState = head gameStates
    let board = currentBoard currentGameState
    let player = currentPlayer currentGameState
    
    -- Parse the game end character, optional whitespace and ensure end of file is reached
    endSymbol <- char '#' <|> char 'D' <|> char 'C'
    _ <- optional (many (string " " <|> eol)) <?> "" -- Make error empty as optional cannot cause error
    _ <- eof <?> "only new lines and spaces after end of game is declared (# or D or C reached)"

    -- Pattern match to the 2 cases and use the appropriate check to validate the parsed symbol
    case endSymbol of
        'D' -> if gameIsDrawn (map currentBoard gameStates)
                    then pure currentGameState {currentPlayer = Nothing}
                    else fail ("Draw declared, but game is not in a draw state.\nGame history:\n" ++ show gameStates)

        '#' -> if gameIsWon board (fromJust player)
                    then pure currentGameState {currentPlayer = Nothing}
                    else fail ("Loss declared, for player " ++ show player ++ " but game is not in a loss state.\n" ++ show currentGameState)
        'C' -> pure currentGameState {needsUserInput = True}

{- Parses a BGN coord and converts it to a coord -}
parseBGNCoord :: Parser Coord
parseBGNCoord = do
    -- Parse the BGN coord
    xBGN <- satisfy isLetter
    yBGN <- satisfy isDigit
    
    -- Convert the coordinate and throw an error if there were any issues
    case convertBGNCoord (xBGN, read [yBGN]) of 
        Just coord -> pure coord 
        Nothing -> fail ("Invalid BGN coord was provided (" ++ [xBGN] ++ [yBGN] ++ ").")

    where
        -- Ensure the x value of the coordinate is valid and then convert it
        convertxBGN xBGN = 
            let x = ord xBGN - 97 
            in if x >= 0 && x <= 3 
                    then Just x 
                    else Nothing

        -- Ensure the y value of the coordinate is valid and then convert it
        convertyBGN yBGN = 
            let y = 4 - yBGN
            in if y >= 0 && y <= 3 
                    then Just y
                    else Nothing

        -- Convert the BGN coordinate to a normal coordiante
        convertBGNCoord (xBGN, yBGN) = do
            x <- convertxBGN xBGN
            y <- convertyBGN yBGN
            pure (x, y)

{- Parses a normal move, validates it and returns the resulting game state -}
parseMove :: [GameState] -> Parser GameState
parseMove (gameState:gameStates) = do
    -- Frequently used values
    let (board:boards) = map currentBoard (gameState:gameStates)
    let player = currentPlayer gameState
    let turnNumber = length (gameState:gameStates) `div` 2 + 1 -- Calculate the turn number as it is no longer provided

    moveCoords <- parseMoveCoords

    -- Validate the player moving the piece is the expected player
    when (player /= getPlayerFromCoord board (fst moveCoords))
        $ fail "Wrong player trying to move piece! Try again!"
    
    let newBoard = uncurry (makeMove board) moveCoords
    -- If the board is nothing, the move was invalid
    when (isNothing newBoard) 
        $ fail "Invalid move entered! Try again!"

    let newBoard' = fromJust newBoard
    -- Check if the player just made a move that won and return the appropriate game state
    if gameIsDrawn (newBoard':board:boards) || gameIsWon newBoard' (fromJust player)
        then pure gameState {currentTurnNumber = turnNumber, currentBoard = newBoard', currentPlayer = Nothing, needsUserInput = False} -- End state 
        else pure gameState {currentTurnNumber = turnNumber, currentBoard = newBoard', currentPlayer = opponent <$> player} -- Non end state

    where 
        -- Parses the player's specified move
        parseMoveCoords = do 
            _ <- char '('
            _ <- char '('
            x1 <- satisfy isDigit 
            _ <- char ','
            y1 <- satisfy isDigit 
            _ <- char ')'
            _ <- char ','
            _ <- char '('
            x2 <- satisfy isDigit 
            _ <- char ','
            y2 <- satisfy isDigit 
            _ <- char ')'
            _ <- char ')'

            pure ((read [x1], read[y1]), (read [x2], read[y2]))

