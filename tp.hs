{-      
 - TRABAJO PRÁCTICO 1
 -
 - CRESPO, Lisandro     ()
 - MISTA, Agustín       (M-6105/1)
-}

--------------------------------- BTREE32 --------------------------------------
data BTree32 k a = Nil | Node (BTree32 k a) Int (k, a) (BTree32 k a)

-- SHOW
instance (Show k, Show a) => Show (BTree32 k a) where
  show =  unlines . layoutTree

indent :: [String] -> [String]
indent = map ("       "++)

layoutTree :: (Show k, Show a) => BTree32 k a -> [String]
layoutTree Nil = [] 
layoutTree (Node l s v r) = 
            indent (layoutTree r) ++ 
            [show (fst v) ++ "->" ++ show (snd v), " s: "++show s] ++ 
            indent (layoutTree l)
         

-- Proyecciones de un BTree32
left :: BTree32 k a -> BTree32 k a
left Nil = Nil
left (Node l _ _ _) = l

right :: BTree32 k a -> BTree32 k a
right Nil = Nil
right (Node _ _ _ r) = r

root :: BTree32 k a -> (k, a)
root (Node _ _ v _) = v

   
-- ROTACIONES
singleL :: BTree32 k a -> BTree32 k a
singleL Nil = Nil
singleL (Node a sx vx (Node b sy vy c)) = 
        (Node (Node a (size a + size b + 1) vx b) sx vy c)

singleR :: BTree32 k a -> BTree32 k a
singleR Nil = Nil
singleR (Node (Node a sx vx b) sy vy c) = 
        (Node a sy vx (Node b (size b + size c + 1) vy c))

doubleL :: BTree32 k a -> BTree32 k a
doubleL Nil = Nil
doubleL (Node a sx vx (Node (Node b sy vy c) sz vz d)) = 
        (Node (Node a (size a + size b + 1) vx b) sx vy (Node c (size c + size d + 1) vz d))

doubleR :: BTree32 k a -> BTree32 k a
doubleR Nil = Nil
doubleR (Node (Node a sx vx (Node b sy vy c)) sz vz d) = 
        (Node (Node a (size a + size b + 1) vx b) sy vy (Node c (size c + size d + 1) vz d))
doubleR x = x

-- BALANCEO
balance :: Ord k => BTree32 k a -> (k, a) -> BTree32 k a -> BTree32 k a
balance l v r 	| size l + size r <= 1	=	Node l (size l + size r + 1) v r
				| size r > 3 * size l	= 	if size (left r) < 2 * size (right r)
											then singleL(Node l (size l + size r + 1) v r)
											else doubleL(Node l (size l + size r + 1) v r)
				| size l > 3 * size r	= 	if size (right l) < 2 * size (left l)
											then singleR(Node l (size l + size r + 1) v r)
											else doubleR(Node l (size l + size r + 1) v r)
				| otherwise				=	Node l (size l + size r + 1) v r


-- VERIFICACIONES
isBalanced :: Ord k => BTree32 k a -> Bool
isBalanced Nil = True
isBalanced (Node Nil s v Nil) = True
isBalanced (Node l s v r) 	| size r == 1 && size l == 0    = True
                            | size r == 0 && size l == 1    = True
                            | size r > (3 * size l)        	= False
                            | size l > (3 * size r)        	= False
                            | otherwise                     = isBalanced l && isBalanced r


isBST :: Ord k => BTree32 k a -> Bool
isBST Nil = True
isBST (Node Nil s v Nil) = True
isBST (Node l s v Nil) = if fst v < fst (root l) then False else isBST l
isBST (Node Nil s v r) = if fst v > fst (root r) then False else isBST r
isBST (Node l s v r)    | fst v < fst (root l) || fst v > fst (root r)     = False
                        | otherwise                                        = isBST l && isBST r 


isSizeOk :: Ord k => BTree32 k a -> Bool
isSizeOk Nil = True
isSizeOk (Node l s v r) = if (s == size l + 1 + size r) 
                            then isSizeOk l && isSizeOk r
                            else False  

isBTree32 :: Ord k => BTree32 k a -> Bool
isBTree32 t = isBalanced t && isBST t && isSizeOk t 


-- OPERACIONES
size :: BTree32 k a -> Int
size Nil = 0
size (Node _ s _ _) = s


bst_lookup :: Ord k => k -> BTree32 k a -> Maybe a
bst_lookup _ Nil = Nothing                                              
bst_lookup k (Node l s v r)     | k == (fst v)  = Just (snd v)          
                                | k < (fst v)   = bst_lookup k l        
                                | k > (fst v)   = bst_lookup k r   


insert :: Ord k => (k, a) -> BTree32 k a -> BTree32 k a
insert x Nil = Node Nil 1 x Nil
insert x@(k, a) t@(Node l s v r)	| k < fst v	= balance (insert x l) v r
									| k > fst v	= balance l v (insert x r)
									| otherwise = t


delRoot :: Ord k => BTree32 k a -> BTree32 k a
delRoot Nil = Nil
delRoot (Node Nil s v Nil) = Nil
delRoot (Node l s v r)  | size l < size r   = let (Node l' s' v' r') = r in balance l  v' (delRoot r)
                        | otherwise         = let (Node l' s' v' r') = l in balance (delRoot l) v' r


delete :: Ord k => k -> BTree32 k a -> BTree32 k a
delete _ Nil = Nil
delete x t@(Node l s v r)   | x == (fst v)  = delRoot t  
                            | x < (fst v)   = balance (delete x l) v r
                            | x > (fst v)   = balance l v (delete x r)


------------------------------ DICCIONARIO -------------------------------------

class Diccionario t where
    vacia       :: Ord k => t k v
    insertar    :: Ord k => (k, v) -> t k v -> t k v
    eliminar    :: Ord k => k -> t k v -> t k v
    buscar      :: Ord k => k -> t k v -> Maybe v
    convertir   :: Ord k => [(k, v)] -> t k v


instance Diccionario BTree32 where
    vacia = Nil
    insertar = insert
    eliminar = delete
    buscar = bst_lookup
    convertir = createFromList
    

createFromList :: Ord k => [(k, a)] -> BTree32 k a
createFromList [] = Nil
createFromList (x:xs) = insert x (createFromList xs)
--------------------------------- DATASETS -------------------------------------

d1  = vacia :: BTree32 Int Char
d2  = insertar (1  , 'c') d1
d3  = insertar (12 , 'f') d2
d4  = insertar (8  , 'e') d3
d5  = insertar (15 , 'h') d4
d6  = insertar (7  , 'i') d5
d7  = insertar (43 , 'j') d6
d8  = insertar (18 , 'k') d7
d9  = insertar (2  , 'l') d8
d10 = insertar (13 , 'm') d9
d11 = insertar (25 , 'n') d10
d12 = insertar (34 , 'o') d11
d13 = insertar (26 , 'p') d12
d14 = insertar (45 , 'q') d13
d15 = insertar (14 , 'r') d14
d   = insertar (46 , 'r') d15

l = [(x, log x)| x <- [1..10]]
t = convertir l :: BTree32 Double Double


x = Nil
x1 = insert (4, 4) x
x2 = insert (2, 4) x1
x3 = insert (3, 4) x2




