import Data.Char

szyfruj list x = [[ chr((ord y)+x) |  y<- wlist ] |  wlist <-list]

deszyfruj list x = [[ chr((ord y)-x) |  y<- wlist ] |  wlist <-list]

--find_key list x = 
--  if is_word head deszyfruj list x
     

is_word word = do
  contents <- readFile "slowa-utf8.txt"
  let wlist =  lines contents
  print $ elem word wlist