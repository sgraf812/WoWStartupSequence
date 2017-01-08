{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
#!/usr/bin/env stack
-- stack --resolver lts-7.15 --install-ghc runghc --package extra

import qualified Data.IntSet as IntSet
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Bits ((.|.), shiftL, testBit, bit, xor, countTrailingZeros, popCount)
import Control.Arrow (first, (&&&))
import Control.Monad (forM_)
import Data.Ord
import Data.Monoid
import Data.List.Extra (chunksOf)

type Field = Int

data WorkItem
  = WorkItem
  { getField :: !Field
  , getSolution :: ![Int]
  , getHeuristic :: !Int
  } deriving Eq

admissableHeuristic :: Field -> [Int] -> Int
admissableHeuristic field solution = (popCount field `div` 3) + length solution

workItem :: Field -> [Int] -> WorkItem
workItem field solution = WorkItem field solution (admissableHeuristic field solution)

-- This speeds up the search considerably.
instance Ord WorkItem where
  compare = comparing getHeuristic <> comparing getField <> comparing getSolution

packPlayingField :: [Int] -> Int
packPlayingField = foldr (.|.) 0 . Seq.mapWithIndex (flip shiftL) . Seq.fromList

initial :: Field
initial = packPlayingField [
    1, 0, 0, 0, 1,
    0, 0, 1, 0, 0,
    0, 1, 1, 1, 0,
    0, 0, 1, 0, 0,
    1, 0, 0, 0, 1
  ]

-- I'm not too sure about this.
isGoalState :: Field -> Bool
isGoalState = (<= 1) . popCount

possibleMoves :: Field -> [Int]
possibleMoves field = map bit . filter (testBit field) $ [0..24]

fromFieldIdx :: (Int, Int) -> Int
fromFieldIdx (x, y) = x * 5 + y

toFieldIdx :: Int -> (Int, Int)
toFieldIdx idx = (idx `div` 5, idx `mod` 5)

applyMove :: Field -> Int -> Field
applyMove field moveBit =
  let moveIdx = countTrailingZeros moveBit
      fromFieldIdx (x, y) = x * 5 + y
      toFieldIdx idx = (idx `div` 5, idx `mod` 5)
      wrapAround (x, y) = (x `mod` 5, y `mod` 5)
      (x, y) = toFieldIdx moveIdx
      toToggle = map wrapAround [(x, y), (x+1, y), (x-1, y), (x, y+1), (x, y-1)]
      newField = foldr (xor . bit . fromFieldIdx) field toToggle
  in newField

solutions :: Field -> [[Int]]
solutions start = map reverse (go (Set.singleton $ workItem start []) IntSet.empty)
  where
    go queue visited = case Set.minView queue of
      Nothing -> error "A solution does exist >:("
      Just (WorkItem field moves _, queue')
        | isGoalState field -> moves : go queue' visited
        | otherwise -> go (enqueueAll (newMoveOptions field) queue') visited'
                        where
                          visited' = IntSet.insert field visited
                          newMoveOptions field =
                            map (uncurry workItem)
                            . filter (not . flip IntSet.member visited' . fst)
                            . map (applyMove field &&& (:moves))
                            . possibleMoves
                            $ field


enqueueAll :: [WorkItem] -> Set WorkItem -> Set WorkItem
enqueueAll items queue = foldr Set.insert queue items

playThroughSolution :: Field -> [Int] -> [Field]
playThroughSolution = scanl applyMove

unpackPlayingField :: Field -> [[Int]]
unpackPlayingField field = chunksOf 5 . map (fromEnum . testBit field) $ [0..24]

showPlayingField :: Field -> String
showPlayingField = unlines . map show . unpackPlayingField

runDistance :: [Int] -> Float
runDistance = dist . map (toFieldIdx . countTrailingZeros)
  where
    dist (a:b:rest) = mag (diff a b) + dist (b:rest)
    dist _ = 0

    diff (x1, y1) (x2, y2) = (fromIntegral (x1 - x2), fromIntegral (y1 - y2))
    mag (x, y) = sqrt (x*x + y*y)

main :: IO ()
main =
  mapM_ (print . (id &&& runDistance)) $ solutions initial

{-  let solution = head (solutions initial)
  forM_ (zip (playThroughSolution initial solution) solution) $ \(field, move) -> do
    putStrLn "Field:"
    putStrLn (showPlayingField field)
    putStrLn "Move:"
    putStrLn (showPlayingField move)
    putStrLn ""
    -}
