--pierwsze 17 nieparzystych

-- take 17[ x | x<-[0..], odd x]

--trojkaty

--[(x,y,z) | x<-[3..17],y<-[3..17],z<-[3..17], x < y + z || y< x+z || z< x+y ]

--prostokatne

--[(x,y,z) | x<-[3..17],y<-[3..17],z<-[3..17], x^2 == y^2 + z^2 ]

--head

head' (x:_ ) = x

--length

length' list = sum([1| _<-list])

--take

--take' x list = (if length list == x then list else take' x init list)

take' x list =
    if length list == x
        then list
    else take' x (init list)


map' fun arg = [fun x| x<-arg]

dodawanie x y =
    if length x == 0
        then y
    else dodawanie (init x) ((last x) : y)
