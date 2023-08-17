--------------------------------------------------------------------------------
-- This is where you should put the main application logic for your program.
--------------------------------------------------------------------------------

import Bean.Types
import Bean.Game
import Bean.Parsers

import Text.Megaparsec 
import Text.Megaparsec.Char

import Data.Maybe ( fromJust, isNothing )
import System.Environment (getArgs)

{- Run using stack run -- "path_to_bgn_file" -}
main :: IO ()
main = do 
    -- Gets the file arguments
    args <- getArgs
    
    case args of
        [] -> putStrLn "No game file specified"
        (gameFile:_) -> do 
            game <- readFile gameFile

            -- Parses the game and prints the game
            case parseGame game of 
                Left err -> putStrLn (errorBundlePretty err) 
                Right (gameState:gameStates) -> do
                    
                    -- If the game is in its finished state, output all the boards
                    if not $ needsUserInput gameState 
                        then print (gameState:gameStates)
                        else do 
                            print gameStates -- latest game state will be output in continue game
                            continueGame (gameState:gameStates)

{- Allows a game to be continued by a user who inputs moves -}
continueGame :: [GameState] -> IO ()
continueGame (gameState:gameStates) = do 
    print gameState
    putStrLn "Enter move in form of ((x1,y2),(x2,y2)):"
    move <- getLine

    -- Parse the move that was input
    case parse (parseMove (gameState:gameStates)) "" move of
        -- Try again if the entered move is invalid
        Left err -> do
            putStrLn $ errorBundlePretty err
            continueGame (gameState:gameStates)

        -- If the move is valid, check if the game is over
        Right newGameState -> 
            if isNothing $ currentPlayer newGameState 
                then print newGameState
                else continueGame (newGameState:gameState:gameStates)




