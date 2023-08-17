{-
You are not required to edit this file, but do feel free to take a look. The 
code in here is implemented in a deliberately poor way, without comments or 
guidance. In my opinion, decoding the approaches taken here is *much* more 
difficult than implementing the work properly yourself :)
-}
{-
Tasty is the testing library that is used to specify tests.
The backends "tasty-hunit" and "tasty-quickcheck" specify the way that unit 
tests and property tests (respectively) are written.
-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}

import Bean.Game
import Bean.Types
import Data.List (sort, tails, uncons, intercalate)
import Data.Maybe (fromJust, isJust)
import System.Console.ANSI (clearScreen)
import Test.Tasty
  ( TestTree, testGroup,
  )
import Test.Tasty.Muffled ( muffledMain )
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Control.Monad (foldM)


main :: IO ()
main = do
  clearScreen
  muffledMain $
    testGroup
      "Tests"
      [ testStartingPos
      , testBalance
      , testInstances
      , testGetPiece
      , testValidCowMoves
      , testValidBeanMoves
      , testSetCoord
      , testMakeMove
      , testGameIsDrawn
      , testGameIsWon
      , testNextMove
      ]

testStartingPos :: TestTree
testStartingPos = testGroup "Ex.1: startingPos"
  [ testCase "Has four rows"
      $ length startingPos @?= 4
  , testCase "Has four pieces in each row"
      $ map length startingPos @?= [4,4,4,4]
  , testCase "Pieces are in the right order"
      $ assertBool "The piece ordering is incorrect in some way."
      $ encode startingPos == 29418847590
  ]

testBalance :: TestTree
testBalance = testGroup "Ex. 2: balance"
  [ testCase "The starting position has balance 0"
      $ balance startingPos @?= 0
  , testCase "After (1,2) takes (1,1), balance is -1"
      $ balance (decode 29414944465) @?= -1
  , testProperty "Balance never exceeds range [-4,4]"
      $ forAll reachablePosition $ \a -> let k = balance a in k > -5 && k < 5
  ]

testInstances :: TestTree
testInstances = testGroup "Ex. 3: Show and Eq instances" 
  [ testCaseSteps  "`show startingPos` prints correctly" 
      $ \step -> do
        let 
          correctPrint = ".\ESC[31mC\ESC[0m\ESC[31mC\ESC[0m.\n\ESC[31mB\ESC[0m\ESC[31mB\ESC[0m\ESC[31mB\ESC[0m\ESC[31mB\ESC[0m\n\ESC[34mb\ESC[0m\ESC[34mb\ESC[0m\ESC[34mb\ESC[0m\ESC[34mb\ESC[0m\n.\ESC[34mc\ESC[0m\ESC[34mc\ESC[0m."
          failed = unlines
            ["\27[0mThe correct result for `show startingPos`"
            , correctPrint
            , "Your result:"
            , show startingPos
            ]
        if show startingPos == correctPrint
          then step (show startingPos)
          else assertFailure failed
  , testProperty "Eq behaves correctly for PieceType"
      $ forAll arbitrary $ \(p :: PieceType) -> 
        forAll arbitrary $ \(q :: PieceType) ->
          (p == q) == (isoPT p == isoPT q)
  , testProperty "Eq behaves correctly for Piece"
      $ forAll arbitrary $ \(p :: Piece) -> 
        forAll arbitrary $ \(q :: Piece) ->
          (p == q) == (isoP p == isoP q)
  ]

testGetPiece :: TestTree
testGetPiece = testGroup "Ex. 4: getPiece"
  [ testCase "getPiece startingPos (0,0) is empty"
      $ getPiece startingPos (0,0) @?= Just Empty
  , testCase "getPiece startingPos (3,2) is a blue bean"
      $ getPiece startingPos (3,2) @?= Just (Blue Bean)
  , testCase "getPiece startingPos (-1,5) is out of bounds"
      $ getPiece startingPos (-1,5) @?= Nothing
  ]

b :: Integer
b = 127071116700

testValidCowMoves :: TestTree
testValidCowMoves = testGroup "Ex. 5: validCowMoves"
  [ testCaseSteps "the grid b" $ \step -> step $ show $ decode b
  , testCase "A cow at (1,3) has 3 available moves from b"
      $ sort (validCowMoves (decode b) (1,3)) @?= [(0,3),(1,2),(2,3)]
  , testCase "A cow at (1,2) has 0 available moves from b"
      $ validCowMoves (decode b) (1,2) @?= []
  ]

testValidBeanMoves :: TestTree
testValidBeanMoves = testGroup "Ex. 6: testValidBeanMoves"
  [ testCase "A Blue bean at (2,2) has 2 available moves from b"
      $ sort (validBeanMoves (decode b) BluePlayer (2,2)) 
        @?= [(1,2),(2,1),(2,3)]
  , testCase "A Red bean at (1,2) has 3 available moves from b"
      $ sort (validBeanMoves (decode b) RedPlayer (1,2)) 
        @?= [(0,2),(1,1),(2,2)]
  ]

testSetCoord :: TestTree
testSetCoord = testGroup "Ex. 7: setCoord"
  [ testCase "setCoord b (0,2) (Red Bean) behaves correctly"
      $ setCoord (decode b) (0,2) (Red Bean) @?= decode 127070726075
  ]

testMakeMove :: TestTree
testMakeMove = testGroup "Ex. 8: makeMove"
  [ testCase "makeMove b (2,0) (3,0) is invalid"
      $ makeMove (decode b) (2,0) (3,0) @?= Nothing
  , testCase "makeMove b (2,0) (1,0) moves correctly"
      $ makeMove (decode b) (2,0) (1,0) @?= Just (decode 127071116640)
  ]

testGameIsDrawn :: TestTree
testGameIsDrawn = testGroup "Ex. 9: gameIsDrawn"
  $
  let 
      p = all (maybe True (uncurry notElem) . uncons) . tails
  in 
  [ testProperty "Short games with no repetitions are not drawn"
      $ forAll (game (0,49) `suchThat` p)  
      $ \(g::[Board]) -> not $ gameIsDrawn g
  , testProperty "Games with at least fifty moves played are drawn"
      $ forAll (game (50,100))
      $ \(g :: [Board]) -> gameIsDrawn g
  , testProperty "Games with repetitions are drawn"
      $ forAll (game (0,100) `suchThat` (not . p))
      $ \(g :: [Board]) -> gameIsDrawn g
  ]
bWin :: Integer

bWin = 126973616700

testGameIsWon :: TestTree
testGameIsWon = testGroup "Ex. 10: gameIsWon"
  [ testCaseSteps "the grid bWin" $ \step -> step $ show $ decode bWin
  , testCase "bWin is won on Red's turn "
      $ gameIsWon (decode bWin) RedPlayer @?= True
  , testCase "bWin is not won on Blue's turn"
      $ gameIsWon (decode bWin) BluePlayer @?= False
  ]

testNextMove :: TestTree
testNextMove = testGroup "Grand Finale: nextMove"
  [ testProperties "nextMove does not attempt to make any illegal moves"
      [ ("...as Blue", 
        forAllShrinkShow (game (0,30)) (filter (not.null) . subterms) (\c -> "\nAll boards, most recent first:\n" ++ show c)
        $ \gs@(g:_) -> 
        let m' = nextMove gs BluePlayer
            k = decode $ appl (encode g) (uncurry Move m')
        in not (null $ nextMoves g BluePlayer) ==> forAllShow (pure k) (\c -> "\nTried to move to: \n" ++ show c)
          $ \_ -> isJust (uncurry (makeMove g) m')
      ), ("...as Red",
        forAllShrinkShow (game (0,30)) (filter (not.null) . subterms) (\c -> "\nAll boards, most recent first:\n" ++ show c)
        $ \gs@(g:_) -> 
        let m' = nextMove gs RedPlayer
            k = decode $ appl (encode g) (uncurry Move m')
        in not (null $ nextMoves g RedPlayer) ==> forAllShow (pure k) (\c -> "\nTried to move to: \n" ++ show c)
          $ \_ -> isJust (uncurry (makeMove g) m')
      )
      ]
  , testProperty "Evaluation of your AI" 
    $ withMaxSuccess 1000
    $ do
      let 

        opp RedPlayer = BluePlayer
        opp BluePlayer = RedPlayer
        oppM AI = Rando
        oppM Rando = AI

        playGame :: Gen Result
        playGame = do
          (first::Mover) <- arbitraryBoundedEnum  
          step [startingPos] BluePlayer first

        step :: [Board] -> Player -> Mover -> Gen Result
        step gs@(g:_) p m = if
          | gameIsWon g p -> pure $ if m == AI then Lose else Win
          | gameIsDrawn gs -> pure Draw
          | otherwise -> do
            g' <- fromJust . uncurry (makeMove g) <$> case m of
              AI -> pure $ nextMove gs p
              Rando -> fmap (\(Move a c) -> (a,c)) $ elements $ nextMoves g p
            step (g':gs) (opp p) (oppM m)

      forAll playGame $ \res -> do
          classify (res == Lose) "Lose" 
        $ classify (res == Draw) "Draw" 
        $ classify (res == Win)  "Win"  
        True

  ]

--------------------------------------------------------------------------------
-- Some garbage to make the tests work properly. 
-- This code is bad on purpose. There are no lessons to learn here.


data Mover = AI | Rando deriving (Eq, Ord, Show, Bounded, Enum)
data Result = Lose | Draw | Win deriving (Eq, Show)
  

data Move = Move Coord Coord deriving (Show,Eq)

encode :: Board -> Integer
encode = foldr (\x n -> n*5 + case x of
  Empty -> 0
  Red Bean -> 1
  Blue Bean -> 2
  Red Cow -> 3
  Blue Cow -> 4
  ) 0 . concat

decode :: Integer -> [[Piece]]
decode j = decode' j 16 [] where
  decode' _ 0 l = reverse (map reverse l)
  decode' k n l = let
    (k',a) = k `quotRem` 5
    p = [Empty, Red Bean, Blue Bean, Red Cow, Blue Cow] !! fromInteger a
    (l':ls) = if n `rem` 4 == 0 then []:l else l
    in decode' k' (n-1) ((p:l'):ls)

rmove :: Board -> Player -> Maybe (Gen Move)
rmove c p = case nextMoves c p of
  [] -> Nothing
  xs -> Just $ elements xs

-- Generate a random move.
nextMoves :: Board -> Player -> [Move]
nextMoves c p = case p of
  RedPlayer -> concat
        [ [ (i, j) `Move` (k, l)
            | (i, j) <- (,) <$> [0 .. 3] <*> [0 .. 3],
              Red Cow <- [concat c !! (j * 4 + i)],
              (k, l) <- (,) <$> [0 .. 3] <*> [0 .. 3],
              (i==k||j==l) && (abs (k-i+j-l)==1),
              Empty <- [concat c !! (l * 4 + k)]
          ],
          [ (i, j) `Move` (k, l)
            | (i, j) <- (,) <$> [0 .. 3] <*> [0 .. 3],
              Red Bean <- [concat c !! (j * 4 + i)],
              (k, l) <- (,) <$> [0 .. 3] <*> [0 .. 3],
              (i==k||j==l) && (abs (k-i+j-l)==1),
              Empty <- [concat c !! (l * 4 + k)]
          ],
          [ (i, j) `Move` (k, l)
            | (i, j) <- (,) <$> [0 .. 3] <*> [0 .. 3],
              Red Bean <- [concat c !! (j * 4 + i)],
              (k, l) <- (,) <$> [0 .. 3] <*> [0 .. 3],
              (i==k||j==l) && (abs (k-i+j-l)==1),
              Blue Bean <- [concat c !! (l * 4 + k)]
          ]
        ]
  BluePlayer -> concat
        [ [ (i, j) `Move` (k, l)
            | (i, j) <- (,) <$> [0 .. 3] <*> [0 .. 3],
              Blue Cow <- [concat c !! (j * 4 + i)],
              (k, l) <- (,) <$> [0 .. 3] <*> [0 .. 3],
              (i==k||j==l) && (abs (k-i+j-l)==1),
              Empty <- [concat c !! (l * 4 + k)]
          ],
          [ (i, j) `Move` (k, l)
            | (i, j) <- (,) <$> [0 .. 3] <*> [0 .. 3],
              Blue Bean <- [concat c !! (j * 4 + i)],
              (k, l) <- (,) <$> [0 .. 3] <*> [0 .. 3],
              (i==k||j==l) && (abs (k-i+j-l)==1),
              Empty <- [concat c !! (l * 4 + k)]
          ],
          [ (i, j) `Move` (k, l)
            | (i, j) <- (,) <$> [0 .. 3] <*> [0 .. 3],
              Blue Bean <- [concat c !! (j * 4 + i)],
              (k, l) <- (,) <$> [0 .. 3] <*> [0 .. 3],
              (i==k||j==l) && (abs (k-i+j-l)==1),
              Red Bean <- [concat c !! (l * 4 + k)]
          ]
        ]

reachablePosition :: Gen Board
reachablePosition = do
  n <- chooseInt (1,10)
  let players = take n $ cycle [BluePlayer, RedPlayer]
      m c p = maybe (pure c) (fmap $ appl c) $ rmove (decode c) p 
  decode <$> foldM m (encode startingPos) players

game :: (Int,Int) -> Gen [Board]
game p = do
  n <- chooseInt p
  let players = take n $ cycle [BluePlayer, RedPlayer]
      m cs@(c:_) pp = maybe (pure cs) (fmap $ (:cs) . appl c) $ rmove (decode c) pp 
  map decode <$> foldM m [encode startingPos] players

appl :: Integer -> Move -> Integer
appl n (Move (i,j) (k,l)) = let
  [u,v,x,y] = map (5^) [(j*4+i)+1,j*4+i,(l*4+k)+1,l*4+k]
  (h,m) = n `quotRem` u
  (w,t) = m `quotRem` v
  (h',m') = (h*u+t) `quotRem` x
  (_ ,t') = m' `quotRem` y
  in h'*x+w*y+t'

-- A morphism between pieces and DPieces (for testing derived equality)
data DPiece = DEmpty | DRed DPieceType | DBlue DPieceType
  deriving Eq
data DPieceType = DBean | DCow
  deriving Eq

isoP :: Piece -> DPiece
isoP Empty = DEmpty
isoP (Red x) = DRed $ isoPT x
isoP (Blue x) = DBlue $ isoPT x

isoPT :: PieceType -> DPieceType
isoPT Cow = DCow
isoPT Bean = DBean

instance Arbitrary PieceType where arbitrary = elements [minBound..maxBound]
instance Arbitrary Piece where arbitrary = elements [Empty,Red Bean, Blue Bean, Red Cow, Blue Cow]
deriving instance Enum PieceType
deriving instance Bounded PieceType

instance {-# OVERLAPS #-} Show [Board] where
  show = intercalate "\n\n" . map show