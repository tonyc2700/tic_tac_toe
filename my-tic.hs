data Move = X | O | N deriving (Eq, Show, Read)
-- X is a cross, O is a nought and N is empty (Nill)
--type Row = [Move] -- A Row is a list of Moves
type Board = [[Move]] -- A Board is a list of Rows

row1 :: [Move] 
row1 = [N, N, N]
row2 :: [Move]
row2 = [N, N, N]
row3 :: [Move]
row3 = [N, N, N]

board :: Board
board = [row1,row2,row3]

showRow :: [Move] -> String
showRow [a, b, c] = show a ++ " | " ++ show b ++ " | " ++ show c

showBoard :: Board -> IO()
showBoard [a, b, c] = putStrLn (showRow a ++ "\n"
                                ++ showRow b ++ "\n"
                                ++ showRow c)

changeCol :: [Move] -> Int -> Move -> [Move]
changeCol c i m = bef ++ (m : aft)
  where bef = take (i-1) c 
        aft = drop (i) c
getRow :: Board -> Int -> [Move]
getRow b r = head (drop (r-1) b)

getelt :: Int -> [a] -> a
getelt i x = (head ( drop (i-1) x ))

--given a (Board row column Move) it selects the row, changes the column
--of that row and replaces the old row with the new one into the board.
--ROWS AND COLUMNS START AT 1 NOT 0!
move :: Board -> Int -> Int-> Move -> Board
move b 0 _ _ = b --Rows and colums begin at 1
move b _ 0 _ = b --Rows and colums begin at 1
move b r c m = bef ++ ((changeCol(getRow b r) c m) : aft)
  where bef = take (r-1) b
        aft = drop (r) b

winner :: Board -> Bool
winner b
-- Horisontal lines
  | (getelt 1 (getRow b 1)) == (getelt 2 (getRow b 1))
    && (getelt 2 (getRow b 1)) == (getelt 3 (getRow b 1)) = True
  | (getelt 1 (getRow b 2)) == (getelt 2 (getRow b 2))
    && (getelt 2 (getRow b 2)) == (getelt 3 (getRow b 2)) = True
  | (getelt 1 (getRow b 3)) == (getelt 2 (getRow b 3))
    && (getelt 2 (getRow b 3)) == (getelt 3 (getRow b 3)) = True
-- Diagonal lines
  | (getelt 1 (getRow b 1)) == (getelt 2 (getRow b 2))
    && (getelt 2 (getRow b 2)) == (getelt 3 (getRow b 3)) = True
  | (getelt 3 (getRow b 1)) == (getelt 2 (getRow b 2))
    && (getelt 2 (getRow b 2)) == (getelt 1 (getRow b 1)) = True
-- Vertical lines
  | (getelt 1 (getRow b 1)) == (getelt 1 (getRow b 2))
    && (getelt 1 (getRow b 2)) == (getelt 1 (getRow b 3)) = True
  | (getelt 2 (getRow b 1)) == (getelt 2 (getRow b 2))
    && (getelt 2 (getRow b 2)) == (getelt 2 (getRow b 3)) = True
  | (getelt 3 (getRow b 1)) == (getelt 3 (getRow b 2))
    && (getelt 3 (getRow b 2)) == (getelt 3 (getRow b 3)) = True

  | otherwise = False