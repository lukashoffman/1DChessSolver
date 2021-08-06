{-# OPTIONS_GHC -Wall #-}
module Chess where 

import Data.List
import Data.Maybe
import Data.Set

data Color = Black | White deriving (Show, Eq, Ord)
data Kind = King | Knight | Rook deriving (Show, Eq) 
data Piece = Piece {color::Color, kind::Kind} deriving (Show, Eq)
data Pos = P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 deriving (Show, Enum, Eq)
data PieceOnBoard = PieceOnBoard Piece Pos deriving (Show, Eq)

adj :: Pos -> (Pos, Pos)
adj P1 = (P2,P2)
adj P2 = (P1,P3)
adj P3 = (P2,P4)
adj P4 = (P3,P5)
adj P5 = (P4,P6)
adj P6 = (P5,P7)
adj P7 = (P6,P8)    
adj P8 = (P7,P7)

prevPosition :: Pos -> Pos
prevPosition pos = fst (adj pos) 

nextPosition :: Pos -> Pos
nextPosition pos = snd (adj pos)

getColor :: Piece -> Color
getColor (Piece c _) = c


nextColor :: Color -> Color 
nextColor Black = White
nextColor White = Black

-- separate piece information from position 
piece :: PieceOnBoard -> Piece
piece (PieceOnBoard p _ ) = p

getPos :: PieceOnBoard -> Pos
getPos (PieceOnBoard _ p) = p

-- return a list of the current pieces on the board
getPieces :: GameState -> [PieceOnBoard]
getPieces (GameState _ pieces) = pieces 

-- track which color's turn it is
getTurnColor :: GameState -> Color
getTurnColor (GameState c _) = c 

-- return a piece from a GameState at a certain position
getPieceFromBoard :: GameState -> Pos -> Maybe PieceOnBoard
getPieceFromBoard (GameState _ []) _ = Nothing 
getPieceFromBoard (GameState _ (a:as)) p = if (getPos a) == p 
                                         then Just a
                                       else getPieceFromBoard (GameState Black as) p

changePiecePos :: PieceOnBoard -> Pos -> PieceOnBoard
changePiecePos tempPiece tempPos = PieceOnBoard (piece tempPiece) tempPos

-- gameState stores current color as well as list of pieces and their positions
data GameState = GameState Color [PieceOnBoard] deriving (Show)


activePieces :: GameState -> [PieceOnBoard] 
activePieces (GameState _ []) = []
activePieces state = [tempPiece | tempPiece <- getPieces state, color (piece tempPiece) == getTurnColor state]

-- we need inactivePieces to check if inactive king is in check 
inactivePieces :: GameState -> [PieceOnBoard]
inactivePieces (GameState _ []) = []
inactivePieces state = [tempPiece | tempPiece <- getPieces state, color (piece tempPiece) /= getTurnColor state]

-- check the color of a piece at a certain position, return Nothing if no piece is there
occupantColor :: GameState -> Pos -> Maybe Color
occupantColor (GameState _ []) _ = Nothing 
occupantColor (GameState _ (a:as)) p = if (getPos a) == p 
                                         then Just (getColor (piece a))
                                       else occupantColor (GameState Black as) p
                                        --Black is placeholder (doesn't matter)

move :: GameState -> PieceOnBoard -> Pos -> GameState
move state tempPiece pos
 | (occupantColor state pos) /= Nothing = replacePiece (GameState (getTurnColor state) (Data.List.delete (fromJust (getPieceFromBoard state pos)) (getPieces state))) tempPiece pos
 | otherwise = replacePiece state tempPiece pos

-- remove taken piece from list of pieces and replace piece with edited position
replacePiece :: GameState -> PieceOnBoard -> Pos -> GameState
replacePiece state tempPiece pos = GameState (nextColor(getTurnColor state)) ((Data.List.delete tempPiece (getPieces state)) ++ [changePiecePos tempPiece pos])

-- for each position, returns corresponding character for piece at that position, "-" if no piece
posToCharacter :: GameState -> Pos -> String
posToCharacter state tempPos
 | getPieceFromBoard state tempPos == Nothing = "-"
 | fromJust (getPieceFromBoard state tempPos) == (PieceOnBoard (Piece White King) tempPos) = "k"
 | fromJust (getPieceFromBoard state tempPos) == (PieceOnBoard (Piece White Knight) tempPos) = "n"
 | fromJust (getPieceFromBoard state tempPos) == (PieceOnBoard (Piece White Rook ) tempPos) = "r"
 | fromJust (getPieceFromBoard state tempPos) == (PieceOnBoard (Piece Black King) tempPos) = "K"
 | fromJust (getPieceFromBoard state tempPos) == (PieceOnBoard (Piece Black Knight) tempPos) = "N"
 | fromJust (getPieceFromBoard state tempPos) == (PieceOnBoard (Piece Black Rook) tempPos) = "R"
 | otherwise = "-"

charToPiece :: Char -> Pos -> PieceOnBoard
charToPiece char tempPos
 | char == 'k' = PieceOnBoard (Piece White King) tempPos
 | char == 'n' = PieceOnBoard (Piece White Knight) tempPos
 | char == 'r' = PieceOnBoard (Piece White Rook) tempPos
 | char == 'K' = PieceOnBoard (Piece Black King) tempPos
 | char == 'N' = PieceOnBoard (Piece Black Knight) tempPos
 | char == 'R' = PieceOnBoard (Piece Black Rook) tempPos

stringToState :: String -> Maybe GameState
stringToState char
 | char !! 0 == 'W' = Just (GameState White [charToPiece (char !! 3) P1, charToPiece (char !! 4) P2, charToPiece (char !! 5) P3, charToPiece (char !! 6) P4, charToPiece (char !! 7) P5, charToPiece (char !! 8) P6, charToPiece (char !! 9) P7, charToPiece (char !! 10) P8])
 | char !! 0 == 'B' = Just (GameState Black [charToPiece (char !! 3) P1, charToPiece (char !! 4) P2, charToPiece (char !! 5) P3, charToPiece (char !! 6) P4, charToPiece (char !! 7) P5, charToPiece (char !! 8) P6, charToPiece (char !! 9) P7, charToPiece (char !! 10) P8])
 | otherwise = Nothing

stateToString :: GameState -> String
stateToString state
 | getTurnColor state == White = "W: " ++ posToCharacter state P1 ++ posToCharacter state P2 ++ posToCharacter state P3 ++ posToCharacter state P4 ++ posToCharacter state P5 ++ posToCharacter state P6 ++ posToCharacter state P7 ++ posToCharacter state P8 
 | getTurnColor state == Black = "B: " ++ posToCharacter state P1 ++ posToCharacter state P2 ++ posToCharacter state P3 ++ posToCharacter state P4 ++ posToCharacter state P5 ++ posToCharacter state P6 ++ posToCharacter state P7 ++ posToCharacter state P8 
 | otherwise = ""

potentialSquares :: GameState -> PieceOnBoard -> [Pos]
potentialSquares state tempPiece = case kind (piece tempPiece) of King -> [tempPos | tempPos <- pairToList (adj (getPos tempPiece)), isValidSquare tempPos state (color(piece(tempPiece))) == True]
                                                                  Knight -> [tempPos | tempPos <- [prevPosition(prevPosition(getPos tempPiece)), nextPosition(nextPosition(getPos tempPiece))], isValidSquare tempPos state (color(piece(tempPiece))) == True]
                                                                  Rook -> [tempPos | tempPos <- (checkLeft state (getPos tempPiece) (color(piece(tempPiece))) []) ++ (checkRight state (getPos tempPiece) (color(piece(tempPiece))) [])]

-- returns true if a square is empty or has a piece of the opposite color 
isValidSquare :: Pos -> GameState -> Color -> Bool
isValidSquare tempPos state tempColor
 | getPieceFromBoard state tempPos == Nothing = True
 | occupantColor state tempPos /= Just tempColor = True
 | otherwise = False

-- iteratively checks each square to the right and adds to move list if valid 
checkRight :: GameState -> Pos -> Color-> [Pos] -> [Pos]
checkRight state tempPos tempColor moveList
 | tempPos == P8 = moveList -- base case (once we reach end of board, return list)
 | occupantColor state (nextPosition tempPos) == Just tempColor = moveList -- if we find a piece of the same color, stop
 | getPieceFromBoard state (nextPosition tempPos) == Nothing = checkRight state ((nextPosition tempPos)) tempColor (moveList ++ [(nextPosition tempPos)])  -- if space is empty, add to list and recurse
 | occupantColor state (nextPosition tempPos) /= Just tempColor = (moveList ++ [(nextPosition tempPos)]) -- if finds an enemy piece, add to list and stop
 | otherwise = moveList

checkLeft :: GameState -> Pos -> Color -> [Pos] ->[Pos]
checkLeft state tempPos tempColor moveList 
 | tempPos == P1 = moveList
 | occupantColor state (prevPosition tempPos) == Just tempColor = moveList
 | getPieceFromBoard state (prevPosition tempPos) == Nothing = checkLeft state ((prevPosition tempPos)) tempColor (moveList ++ [(prevPosition tempPos)]) 
 | occupantColor state (prevPosition tempPos) /= Just tempColor = (moveList ++ [(prevPosition tempPos)])
 | otherwise = moveList

-- check if opposite king's location is a valid move
kingInCheck :: GameState -> Bool
kingInCheck state = (find(\moveList -> moveList == (getPos (getOppositeKing state))) (concat [potentialSquares state tempPieces | tempPieces <- activePieces state])) /= Nothing

-- find King in list of inactive pieces
getOppositeKing :: GameState -> PieceOnBoard
getOppositeKing state = fromJust (find(\pieceList -> kind (piece pieceList) == King) (inactivePieces state))

legalMoves :: GameState -> [GameState]
legalMoves state = [move state tempPiece tempPos | tempPiece <- activePieces state, tempPos <- potentialSquares state tempPiece, kingInCheck(move state tempPiece tempPos) == False]

--There are 1241 possible states from the initialState. The program took approximately 14 seconds
possibleStates :: GameState -> Int
possibleStates state = Data.Set.size (possibleStatesHelper [state] Data.Set.empty)

--Possible states recursive function takes a list of GameStates initialized with the first gamestate, a set of visited gamestates, and an int to track the number of visited states

possibleStatesHelper :: [GameState] -> Set String -> Set String
possibleStatesHelper state set
 | Data.List.null state == True = set
 | otherwise = possibleStatesHelper (Data.List.drop 1 state ++ [tempState | tempState <- legalMoves (state !! 0), (Data.Set.notMember (stateToString tempState) set) == True]) (Data.Set.insert (stateToString (state !! 0)) set)

-- name is self-explanatory
pairToList :: (Pos, Pos) -> [Pos] 
pairToList (p1, p2) = [p1, p2] 

initialState :: GameState
initialState = GameState White [(PieceOnBoard (Piece White King) P1), (PieceOnBoard (Piece White Knight) P2), (PieceOnBoard (Piece White Rook) P3), (PieceOnBoard (Piece Black Rook) P6), (PieceOnBoard (Piece Black Knight) P7), (PieceOnBoard (Piece Black King) P8)]

