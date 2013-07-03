type Board = String

test :: Board
test = "012345678"

showBoard :: Board -> String
showBoard b =
  b !! 0 : [] ++ "_|_" ++ b !! 1 : [] ++ "_|_" ++ b !! 2 : [] ++ "\n" ++ 
  b !! 3 : [] ++ "_|_" ++ b !! 4 : [] ++ "_|_" ++ b !! 5 : [] ++ "\n" ++ 
  b !! 6 : [] ++ " | " ++ b !! 7 : [] ++ " | " ++ b !! 8 : []

  --Given a board a player and a position, returns a
  --new board with a move applied.
move :: Board -> Char -> Int -> Board
move (p:b) ch pos
  | pos > 0 = p:[] ++ (move b ch (pos - 1))
  | otherwise = ch:[] ++ b

-- Checks if the board has a winning player
winningBoard :: Board -> Char
winningBoard b
  --Horizontal lines
  | (b !! 0) /= ' ' && ((b !! 0) == (b !! 1) && (b !! 0) == (b !! 2)) = b !! 0
  | (b !! 3) /= ' ' && ((b !! 3) == (b !! 4) && (b !! 3) == (b !! 5)) = b !! 3
  | (b !! 6) /= ' ' && ((b !! 6) == (b !! 7) && (b !! 6) == (b !! 8)) = b !! 6
  --Vertical lines
  | (b !! 0) /= ' ' && ((b !! 0) == (b !! 3) && (b !! 0) == (b !! 6)) = b !! 0
  | (b !! 1) /= ' ' && ((b !! 1) == (b !! 4) && (b !! 1) == (b !! 7)) = b !! 1
  | (b !! 2) /= ' ' && ((b !! 2) == (b !! 5) && (b !! 2) == (b !! 8)) = b !! 2
  --Diagonal lines
  | (b !! 0) /= ' ' && ((b !! 0) == (b !! 4) && (b !! 0) == (b !! 8)) = b !! 0
  | (b !! 2) /= ' ' && ((b !! 2) == (b !! 4) && (b !! 2) == (b !! 6)) = b !! 2
  -- no winner
  | otherwise = ' '
