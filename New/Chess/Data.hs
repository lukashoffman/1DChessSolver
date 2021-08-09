{-# OPTIONS_GHC -Wall #-}
module Chess.Data
    ( nextPos,
      prevPos,
      PieceOnBoard,
      MailBox,
      GameState,
      Piece,
      Color(..),
      PieceKind(..),
      color, changeColor,
      pieces,
      move,
      occupantColor ) where
import Chess.Abstract ( Ar8, Pos, allPos, getSquare, setSquare, adjPos )
import Data.Maybe

data PieceKind = King | Knight | Rook deriving (Show, Ord, Eq)
data Color = Black | White deriving (Show, Ord, Eq)
type Piece = (PieceKind,Color)
type MailBox = Ar8 (Maybe Piece)
type PieceOnBoard = (Piece, Pos)
type GameState = (MailBox, Color)

color :: PieceOnBoard -> Color
color ((_pk,c),_ps) = c

changeColor :: Color -> Color
changeColor Black = White
changeColor White = Black

pieces :: MailBox -> [PieceOnBoard]
pieces mb = catMaybes $ map (\x -> flip (,) x <$> getSquare mb x) allPos

-- | occupantColor gets the color of the piece on a square, if any
occupantColor :: MailBox -> Pos -> Maybe Color
occupantColor mb p = case getSquare mb p of
                        Nothing -> Nothing 
                        Just (_,c) -> Just c

move :: MailBox -> PieceOnBoard -> Pos -> MailBox 
move mb pob to = setSquare (setSquare mb (snd pob) Nothing) to (Just $ fst pob)

-- | nextPos and prevPos give the next/previous position on the board, if any.
nextPos, prevPos :: Pos -> Maybe Pos
nextPos = fst . adjPos
prevPos = snd . adjPos