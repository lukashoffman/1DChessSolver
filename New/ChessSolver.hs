{-# OPTIONS_GHC -Wall #-}
module ChessSolver where
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set

import Chess.Rules
import Chess.Data


winning :: GameState -> Maybe Color
winning (state, c)
 | Data.List.null (legalMoves (state,c)), kingAttacked c state = Just (changeColor c)
 | otherwise = Nothing

--winning :: GameState -> Maybe Color 
--winning (_, c) = Just c

toMove :: GameState -> Color
toMove (_, c) = changeColor c

swapColor :: GameState -> GameState
swapColor (list, White) = (list, Black)
swapColor (list, Black) = (list, White)

getAllStates :: [GameState]
getAllStates = nub (helper (Set.empty) [initialBoard])
  where
     helper :: Set.Set GameState -> [GameState] -> [GameState]
     helper lookedAt [] = Set.toList lookedAt
     helper lookedAt (e:es) | Set.member e lookedAt = helper lookedAt es
       | otherwise = helper (Set.insert e lookedAt) (es ++ legalMoves e)

categorizeStates :: Map GameState (Color, Int)
categorizeStates = helper getAllStates (Map.fromList [(st,(c,0)) | st <- getAllStates, Just c <- [winning st]])
  where
     helper :: [GameState] -> Map GameState (Color, Int) -> Map GameState (Color, Int)
     helper unprocessed processed
      = case findNodeToProcess unprocessed processed of
           (unprocessed', processed') | length unprocessed == length unprocessed'-> processed
             | otherwise -> helper unprocessed' processed'
findNodeToProcess :: [GameState] -> Map GameState (Color, Int)
                  -> ([GameState], Map GameState (Color, Int))
findNodeToProcess [] processed = ([],processed) 
findNodeToProcess (x:xs) processed
 = let player st = toMove st
       nextStates = legalMoves x
       processedStates = [ (st,c,i) | st <- nextStates, Just (c,i) <- [Map.lookup st processed]]
       winnin = [(st,c,i) | (st,c,i) <- processedStates, c == player st]
       losing = [(st,c,i) | (st,c,i) <- processedStates, c /= player st]
       result = if null winnin
                then (if length losing == length nextStates && not (null (losing))
                      then Just (pickWorstMove losing)
                      else Nothing)
                else Just (pickBestMove winnin)
   in (case result of
              Nothing -> let (xs',processed') = findNodeToProcess xs processed
                         in (x:xs',processed')
              Just (_,c,i) -> let (xs',processed') = findNodeToProcess xs (Map.insert x (c,i) processed) -- SJ: during the lecture, I wrote Map.insert st here, but I of course should add the state that we processed to the map, not the state that we're going to visit.
                         in (xs',processed'))
  where
        pickMove opt lst =
          let minNr = opt (map trd lst)
              (st,c,i) = pickNr minNr lst
          in (st,c,i+1)
        pickWorstMove = pickMove maximum
        pickBestMove = pickMove minimum
        pickNr i ((st,c,i'):sts) | i == i' = (st,c,i')
          | otherwise = pickNr i sts
        pickNr _i [] = error "pickNr has been given a number not in the list"
        trd (_,_,i) = i
        
