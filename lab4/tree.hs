import Data.Char
import Data.String

data Tree a= Empty | Node a(Tree a) (Tree a) deriving (Eq, Ord, Read)

empty Empty = True 
empty _ = False

insert elem Empty = 
   Node elem Empty Empty
   
insert elem (Node x left right) 
  |x >= elem = (Node x left (insert elem right))
  |x < elem = (Node x (insert elem left) right)
              
toString Empty = 
  putStr "Empty"
toString (Node x left right)= do
  putStr( show x ++ "(")  
  toString left
  putStr( "),(")
  toString right
  putStr(")")
  
  
search x Empty = False
search x (Node y left right)
  | x == y = True
  | x > y = search x left
  | x < y = search x right

 
nnodes Empty = 0
nnodes (Node k left right)=
  1 + nnodes left + nnodes right
  
  
leaves Empty = []
leaves (Node k Empty Empty)=[k]
leaves (Node k left right)= 
   leaves left ++ leaves right
  
nsum Empty = 0
nsum (Node k left right)=
  k + nsum left + nsum right
  
vlr Empty = []
vlr(Node v left right)=
  [v] ++ vlr left ++ vlr right

lvr Empty = []
lvr (Node v left right)=
  lvr left ++ [v] ++ lvr right
  
lrv Empty = []
lrv (Node v left right)=
  lrv left ++ lrv right ++ [v]
  
vrl Empty = []
vrl(Node v left right)=
  [v] ++ vrl right ++ vrl left
  
rvl Empty = []
rvl (Node v left right)=
  rvl right ++ [v] ++ rvl left
  
rlv Empty = []
rlv (Node v left right)=
  rlv right ++ rlv left ++ [v]
  
tmap fun (Node v left right) =
  [ fun x| x<-(vlr (Node v left right))]
  
getLevel x Empty = []
getLevel 0 (Node x _ _) = [x]
getLevel x (Node v left right)=
  getLevel (x-1) left ++ getLevel (x-1) right
  
listString list = 
  [show x| x<-list]

dumpDOT (Node v left right)=
  "digraph G{ \n"++ dumpNodes ( Node v left right) ++"}\n"
  
dumpNode Empty = ""
dumpNode (Node v _ _) = show v
dumpNodes ( Node v Empty Empty) = ""
dumpNodes ( Node v l Empty)= (show v) ++ "->" ++ (dumpNode l) ++ "\n" ++ (dumpNodes l)
dumpNodes ( Node v Empty r) = (show v) ++ "->" ++ (dumpNode r) ++ "\n" ++ (dumpNodes r)
dumpNodes ( Node v l r) = (show v) ++ "->" ++ (dumpNode l) ++ "\n" ++ (show v) ++ "->" ++ (dumpNode r) ++ "\n" ++ (dumpNodes l) ++ (dumpNodes r)

--writeFile "file.txt" dumpDOT x

fromList [] = Empty
fromList list=
  foldr insert Empty list
  
isBinary :: Tree a -> Bool
isBinary _ = True

-- lab4 


-- zadanie 1

instance Show a => Show (Tree a) where
  show Empty = "Empty"
  show (Node v Empty Empty) = show v
  show (Node v l r) = (show v) ++ "(" ++ (show l) ++ "),(" ++ (show r) ++ ")"
  
  
  
-- zadanie 2

data STree a = Empty2 | Leaf a | Branch a (STree a) (STree a) deriving (Show)

convertFrom Empty2 = Empty
convertFrom (Leaf a) = (Node a Empty Empty)
convertFrom (Branch v l r) = (Node v (convertFrom l) (convertFrom r))

convertTo Empty = Empty2
convertTo (Node v Empty Empty) = Leaf v
convertTo (Node v l r) = (Branch v (convertTo l) (convertTo r))
  
