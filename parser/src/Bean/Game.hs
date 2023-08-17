{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Bean.Game where

--------------------------------------------------------------------------------
-- This file contains implementations of the functions from coursework 1.
--
-- Feel free to use them for this coursework! 
--
-- You are also welcome to ignore them or implement your own alternatives, 
-- if you would prefer to use other data structures or a different approach.
--------------------------------------------------------------------------------

import Bean.Types
import Data.Monoid ( Sum(getSum) )
import Data.Maybe ( fromJust, isNothing )
import Data.List ( tails, uncons )


{-| 
  The starting position for the game.
-}
startingPos :: Board
startingPos = [[Empty,Red Cow, Red Cow,Empty]
  , replicate 4 (Red Bean)
  , replicate 4 (Blue Bean)
  , [Empty, Blue Cow, Blue Cow, Empty]]


{-|
  The balance of material, defined as difference between the 
  number of Red beans and the number of Blue beans.
-}
balance :: Board -> Int
balance = getSum . foldMap c . concat
 where c = \case { Red Bean -> 1; Blue Bean -> -1; _ -> 0}


{-| 
  Gets a piece at a coordinate. 
  If the location is off the board, returns Nothing, otherwise returns the 
  appropriate piece (or Empty), wrapped in a Just.
-}
getPiece :: Board -> Coord -> Maybe Piece
getPiece b (x,y) 
 | x < 0 || x > 3 || y < 0 || y > 3 = Nothing
 | otherwise                        = Just (b !! y !! x)


{-| 
  Returns the valid moves that a cow in position (x,y) would have.
-}
validCowMoves :: Board -> Coord -> [Coord]
validCowMoves b c = filter (allow [Empty] b) $ moves c


{-| 
  Returns the valid moves for a bean on the given team in position c.
-}
validBeanMoves :: Board -> Player -> Coord -> [Coord]
validBeanMoves b RedPlayer c = filter (allow [Empty, Blue Bean] b) $ moves c
validBeanMoves b BluePlayer c = filter (allow [Empty, Red Bean] b) $ moves c

moves :: (Num a, Num b) => (a, b) -> [(a, b)]
moves (x,y) = [ (x+1,y),(x-1,y),(x,y-1),(x,y+1) ]

allow :: Foldable t => t Piece -> Board -> Coord -> Bool
allow ps b c = case getPiece b c of
  Nothing -> False
  Just x  -> x `elem` ps


{-| 
  Sets a given (valid) coordinate to have a given piece (or Empty).
-}
setCoord :: Board -> Coord -> Piece -> Board
setCoord b (x,y) p = let 
  (j,k:l) = splitAt y b
  (u,_:w) = splitAt x k
  in (j ++ (u ++ (p:w)):l)


{-| 
  Ex. 8: Given two positions, returns Just the updated board after moving the
  piece from the first position to the second. If the move was not valid, or 
  there was no piece in the first position, returns Nothing instead.
-}
makeMove :: Board -> Coord -> Coord -> Maybe Board
makeMove b (x1,y1) (x2,y2)
  | isLegal   = Just moved
  | otherwise = Nothing
  where 
    isLegal :: Bool
    isLegal = case getPiece b (x1,y1) of
      Nothing          -> False
      Just Empty       -> False
      Just (Red Bean)  -> (x2,y2) `elem` validBeanMoves b RedPlayer (x1,y1)
      Just (Blue Bean) -> (x2,y2) `elem` validBeanMoves b BluePlayer (x1,y1)
      Just _           -> (x2,y2) `elem` validCowMoves b (x1,y1)
    
    moved = (\b -> setCoord b (x1,y1) Empty) $ setCoord b (x2,y2) p
    p = fromJust $ getPiece b (x1,y1)


{-| 
  The game is drawn if the same setup is repeated, or after the fiftieth 
  move. Given a sequence of boards, most recent first, determine whether the 
  game is drawn in any position.
-}
gameIsDrawn :: [Board] -> Bool
gameIsDrawn bs = not (p bs) || length bs > 50
  where p = all (maybe True (uncurry notElem) . uncons) . tails


{-| 
  The game is won by Red if, on Blue's turn, one of their cows is 
  unable to move - and vice versa. Given a board, and the player whose move it 
  is, determines whether the game is won by the opponent.
-}
gameIsWon :: Board -> Player -> Bool
gameIsWon b p = any (null . validCowMoves b) cowPositions
  where 
    cowPositions = filter (\c -> getPiece b c == Just pc) allPositions
    pc = case p of
      RedPlayer  -> Red Cow
      BluePlayer -> Blue Cow
    allPositions = [ (x,y) | x <- [0..3], y <- [0..3] ]

{- Gets the opponent for the specified player -}
opponent :: Player -> Player
opponent player = case player of
     RedPlayer -> BluePlayer
     BluePlayer -> RedPlayer

{- Gets the player that should be moving the piece at a specified coordinate -}
getPlayerFromCoord :: Board -> Coord -> Maybe Player
getPlayerFromCoord board coord = do 
    piece <- getPiece board coord
    case piece of 
        Empty -> Nothing 
        Red Cow -> Just RedPlayer
        Red Bean -> Just RedPlayer
        Blue Cow -> Just BluePlayer
        Blue Bean -> Just BluePlayer