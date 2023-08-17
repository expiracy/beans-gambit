module Bean.Types where

import Data.Char (toLower)
import qualified Data.List as List

--------------------------------------------------------------------------------
-- These are the types as they appeared in Coursework 1 (with some extra things -- derived or other instances provided for you.)

-- You are under no obligation to use these, 
-- especially if you come up with some better representation!
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Type Definitions

-- | The board is a two-dimensional list of pieces.
type Board = [[Piece]]

instance {-# OVERLAPS #-} Show Board where
  show b = List.intercalate "\n" $ map (concatMap show) b

-- | A coordinate represents the (row,column) of the square in question. 
--   Zero-indexed from the top-left of the board.
type Coord = (Int,Int)

-- | A Piece is one square of the board; it may be empty or contain a Red
--   piece or a Blue piece.
data Piece = Empty | Red PieceType | Blue PieceType
  deriving (Eq)

-- | The corresponding player is either Red or Blue.
data Player = RedPlayer | BluePlayer
  deriving (Eq, Ord)

-- | There are two pieces: Beans and Cows.
data PieceType
  = Bean
  | Cow
  deriving (Eq, Ord)


--------------------------------------------------------------------------------
-- Provided instances

-- You may make use of these instances as appropriate when implementing your 
-- own instances as required.


instance Show PieceType where
  show Bean = "B"
  show Cow  = "C"

instance Show Piece where
  show Empty     = "."
  show (Red pt)  = "\27[31m" ++              show pt ++ "\27[0m"
  show (Blue pt) = "\27[34m" ++ map toLower (show pt) ++ "\27[0m"

instance Show Player where
  show RedPlayer = "\27[31mRed\27[0m player"
  show BluePlayer = "\27[34mBlue\27[0m player"

{- Stores the state of the game after each parsed turn -}
data GameState = GameState 
    { currentTurnNumber :: Int
    , currentBoard :: Board
    , currentPlayer :: Maybe Player -- Is nothing when the game has ended (no player can move)
    , needsUserInput :: Bool } 

{- Custom instance of show outputs game -}
instance {-# OVERLAPS #-} Show GameState where
    show gameState = 
        "Turn number: " ++ show (currentTurnNumber gameState) ++
        "\nPlayer turn: " ++ show (currentPlayer gameState) ++
        "\nBoard:\n" ++ show (currentBoard gameState) ++ "\n"

{- Custom instance of show outputs all -}
instance {-# OVERLAPS #-} Show [GameState] where
    show gameStates = List.intercalate "\n" $ map show $ reverse gameStates

{- Converts the maybe player to the correct output form -}
instance {-# OVERLAPS #-} Show (Maybe Player) where
    show = maybe "Game has ended!" show