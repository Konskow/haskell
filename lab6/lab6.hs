
is_word word = do
  contents <- readFile "slowa-utf8.txt"
  let wlist =  lines contents
  if (elem word wlist)
    then (print (punktuj word))
    else (print "Not a word")
  
punktuj word = foldr (+) 0 (map punkt word)

punkt x
  | x == 'a' || x == 'i' || x == 'e' || x == 'o' || x == 'n' =1
  | x == 'r' || x == 't' || x == 'l' || x == 's' || x == 'u' =1
  | x == 'd' || x == 'g' = 2
  | x == 'b' || x == 'c' || x == 'm' || x == 'p' = 3 
  | x == 'f' || x == 'h' || x == 'v' || x == 'w' || x == 'y' = 4
  | x == 'k' = 5
  | x == 'j' || x =='x' = 8
  | x == 'q' || x == 'z' =10
                                                               
                                                               
  
main = do
  print "Podaj slowo"
  line <- getLine
  if (length line) > 0
    then is_word line
    else return()
  if (length line) >0
    then main
    else print "exit"
 
  

    
  