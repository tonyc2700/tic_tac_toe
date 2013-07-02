type Board = String

board :: Board
board = "12345678"

showBoard :: Board -> String
showBoard b =
  b !! 0 : [] ++ " | " ++ b !! 1 : [] ++ " | " ++ b !! 2 : []
  --b !! 3 : [] ++ " | " ++ b !! 4 : [] ++ " | " ++ b !! 5 : [] " \n" ++
  --b !! 6 : [] ++ " | " ++ b !! 7 : [] ++ " | " ++ b !! 8 : [] " \n" ++