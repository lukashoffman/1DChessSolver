module Chess.Rules where
import Chess.Abstract ( Pos, setupBoard )
import Chess.Data
    ( nextPos,
      prevPos,
      PieceOnBoard,
      MailBox,
      Color(..),
      PieceKind(..),
      color,
      pieces,
      move,
      occupantColor, GameState, changeColor )
import Data.Maybe ( catMaybes )

potentialNextMoves :: MailBox -> PieceOnBoard -> [Pos]
potentialNextMoves mb ((King,c),ps) = filter ((/= Just c) . occupantColor mb) $ catMaybes [nextPos ps, prevPos ps]
potentialNextMoves mb ((Knight,c),ps) = filter ((/= Just c) . occupantColor mb) $ catMaybes [nextPos ps >>= nextPos, prevPos ps >>= prevPos]
potentialNextMoves mb ((Rook,c),ps) = moveFrom nextPos ++ moveFrom prevPos
  where moveFrom f = stopOnPiece f (f ps)
        stopOnPiece f (Just ps')
         = case occupantColor mb ps' of
             Nothing -> ps':stopOnPiece f (f ps')
             Just c2 | c == c2   -> []
                     | otherwise -> [ps']
        stopOnPiece _f Nothing = []

-- | Check if a square indicated by Pos is attacked by the enemy (not 'Color')
isAttacked :: MailBox -> Color -> Pos -> Bool
isAttacked mb c p
 = or [p `elem` (potentialNextMoves mb pob) | pob@((_,pc),_) <- pieces mb, pc /= c]

kingAttacked :: Color -> MailBox -> Bool
kingAttacked c mb
 = isAttacked mb c kingPos
 where kingPos = head [ps | ((King,c'),ps) <- pieces mb, c'==c]

legalMoves :: GameState -> [GameState]
legalMoves (mb,c) = map (flip (,) (changeColor c)) $ filter (not . kingAttacked c) $
                    concatMap (\x -> map (move mb x) (potentialNextMoves mb x)) mypieces
  where mypieces = [p | p <- pieces mb, color p == c]

-- | Determine who the winner is in a certain state.
--   note that this can only be the player whose turn it isn't.
won :: GameState -> Maybe Color
won (mb,c)
 = if kingAttacked c mb
   then case legalMoves (mb,c) of
          [] -> Just (changeColor c)
          _ -> Nothing
   else Nothing

initialBoard :: GameState
initialBoard = (setupBoard
                   (white King)
                   (white Knight)
                   (white Rook)
                   Nothing 
                   Nothing 
                   (black Rook)
                   (black Knight)
                   (black King)
                   , White)
  where white x = Just (x,White)
        black x = Just (x,Black)