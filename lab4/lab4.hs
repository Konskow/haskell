--Zadanie 1

instance Show a => Show(Tree a) where
  show (Node v l r) = "nic"


--Zadanie 2


--Zadanie 3

sum' list = foldr (+) 0 list
sum'' list = foldl (+) 0 list

product' list = foldr (*) 1 list
product'' list = foldl (*) 1 list

and' list = foldl (&&) True list
and'' list = foldr (&&) True list

or' list = foldl (||) False list
or'' list = foldr (||) False list

reverse' list = foldl (flip (:)) [] list
reverse'' list = foldr (\y ys-> ys ++ [y]) [] list



