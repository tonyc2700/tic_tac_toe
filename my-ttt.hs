type Board = String

test :: Board
test = "123456789"

showBoard :: Board -> String
showBoard b =
  b !! 0 : [] ++ "_|_" ++ b !! 1 : [] ++ "_|_" ++ b !! 2 : [] ++ "\n" ++ 
  b !! 3 : [] ++ "_|_" ++ b !! 4 : [] ++ "_|_" ++ b !! 5 : [] ++ "\n" ++ 
  b !! 6 : [] ++ " | " ++ b !! 7 : [] ++ " | " ++ b !! 8 : []
