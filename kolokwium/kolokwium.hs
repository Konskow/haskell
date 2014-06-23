wielomian list arg = foldl (+) 0 (map (\ x -> arg^((length list) - (fst x)-1) * (snd x)) (zip [0..] list))
  

--odwrocenie listy
reverse' l =
  foldl (\a x -> x : a) [] l
reverse'' l =
  foldr (\x a -> a ++ (x:[])) [] l
  
--and
and' l =
  foldl (&&) True l
  
  
  
  --kropka

f x = x+3
g x = x*3

funkcja p d = p / d

flip' f p d = f d p



data Piotr a= Konsek | Matyasik | Empty

instance Show a=> Show (Piotr a) where
  show Konsek = "Konsek"
  show Matyasik = "Matyasik"
  show Empty = "Empty"
  
  
  
  

  
  


($$) :: (a -> b) -> (a -> b)
infixl 0 $$
f $$ x = f  x


reverse''' = foldl (\acc x -> x : acc) []


headl (x:xs) = foldl (\a x -> a) x (x:xs) 
taill = foldl (\a x -> x) 0

headr list = foldr (\x a -> x) 0 list


usunU line = 
  filter (\x-> x/= 'u') line



main = do
  line <- getLine
  putStrLn $ usunU line
  
