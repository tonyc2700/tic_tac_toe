type Board = String

board :: Board
board = "12345678"

showBoard :: Board -> String
showBoard b =
  b !! 0 : [] ++ " | " ++ b !! 1 : [] ++ " | " ++ b !! 2 : []