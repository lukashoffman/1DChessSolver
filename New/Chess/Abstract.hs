{-# OPTIONS_GHC -Wall #-}
module Chess.Abstract 
  (Pos,Ar8,getSquare,setSquare,adjPos,setupBoard,allPos) where
-- abstract size-8 board, you can put whatever you want in the squares (probably 'Maybe Piece')
data Pos = A | B | C | D | E | F | G | H deriving (Show, Ord, Eq, Enum)
data Ar8 a = Ar8 a a a a a a a a deriving (Show, Ord, Eq)

allPos :: [Pos]
allPos = [A ..]

-- | You may use this to set up your initial board position
setupBoard :: a -> a -> a -> a -> a -> a -> a -> a -> Ar8 a
setupBoard = Ar8

-- | You can use this to inform yourself on what is on a certain square
getSquare :: Ar8 a -> Pos -> a
getSquare (Ar8 t _ _ _ _ _ _ _) A = t
getSquare (Ar8 _ t _ _ _ _ _ _) B = t
getSquare (Ar8 _ _ t _ _ _ _ _) C = t
getSquare (Ar8 _ _ _ t _ _ _ _) D = t
getSquare (Ar8 _ _ _ _ t _ _ _) E = t
getSquare (Ar8 _ _ _ _ _ t _ _) F = t
getSquare (Ar8 _ _ _ _ _ _ t _) G = t
getSquare (Ar8 _ _ _ _ _ _ _ t) H = t

-- | You can use this to change what is on a certain square
setSquare :: Ar8 a -> Pos -> a -> Ar8 a
setSquare (Ar8 _a  b  c  d  e  f  g  h) A t = (Ar8  t  b  c  d  e  f  g  h)
setSquare (Ar8  a _b  c  d  e  f  g  h) B t = (Ar8  a  t  c  d  e  f  g  h)
setSquare (Ar8  a  b _c  d  e  f  g  h) C t = (Ar8  a  b  t  d  e  f  g  h)
setSquare (Ar8  a  b  c _d  e  f  g  h) D t = (Ar8  a  b  c  t  e  f  g  h)
setSquare (Ar8  a  b  c  d _e  f  g  h) E t = (Ar8  a  b  c  d  t  f  g  h)
setSquare (Ar8  a  b  c  d  e _f  g  h) F t = (Ar8  a  b  c  d  e  t  g  h)
setSquare (Ar8  a  b  c  d  e  f _g  h) G t = (Ar8  a  b  c  d  e  f  t  h)
setSquare (Ar8  a  b  c  d  e  f  g _h) H t = (Ar8  a  b  c  d  e  f  g  t)

-- | This function tells you which positions are adjacent to a certain position
adjPos :: Pos -> (Maybe Pos, Maybe Pos)
adjPos A = (Nothing, Just B )
adjPos B = (Just A , Just C )
adjPos C = (Just B , Just D )
adjPos D = (Just C , Just E )
adjPos E = (Just D , Just F )
adjPos F = (Just E , Just G )
adjPos G = (Just F , Just H )
adjPos H = (Just G , Nothing)