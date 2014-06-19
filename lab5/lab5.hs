

--f a = 3 * a

f a = ( "napis f", 3+a)

--g b = 5*b

g b = ( "napis g", b+2)



--type String = [Char]

sklej::([Char], a) -> (a -> ([Char], b)) -> ([Char],b)
--sklej (fun1, fun2)  = fst( fun1 )
sklej (a, b) fun=   (a  ++ fst(fun b),  snd(fun b))


--nic a `sklej` f == f  a 
--a `sklej` nic == a

nic a = ("", a)

sklejo (a,b) fun = (fst(fun b), snd(fun b))


-- to jest troche zle
bez_info a = 10*a

--dodatkowa::(a->a)-> a -> ([Char], a)
dodatkowa fun = nic . fun