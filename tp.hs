data BTree32 k a = Nil | Node (BTree32 k a) Int (k, a) (BTree32 k a)

class Diccionario t where
    vacia       :: Ord k => t k v
    insertar    :: Ord k => (k, v) -> t k v -> t k v
    eliminar    :: Ord k => k -> t k v -> t k v
    buscar      :: Ord k => k -> t k v -> Maybe v


-- Instancia de BTree32 para Diccionario
instance Diccionario BTree32 where
    vacia = Nil
    insertar = insert
    eliminar = delete
    buscar = bst_lookup

-- Instancia de Show para BTree32
instance (Show k, Show a) => Show (BTree32 k a) where
  show =  unlines . layoutTree

indent :: [String] -> [String]
indent = map ("       "++)

layoutTree :: (Show k, Show a) => BTree32 k a -> [String]
layoutTree Nil = [] 
layoutTree (Node l s v r) 
         = indent (layoutTree r) ++ [show (fst v) ++ "->" ++ show (snd v), " s: "++show s] ++ indent (layoutTree l)
         

-- Retorna el tamaño de un BSTreee32
size :: BTree32 k a -> Int
size Nil = 0
size (Node _ s _ _) = s

-- Busca recursivamente un elemento en un BTree32
bst_lookup :: Ord k => k -> BTree32 k a -> Maybe a
bst_lookup _ Nil = Nothing                                              
bst_lookup k (Node l s v r)     | k == (fst v)  = Just (snd v)          
                                | k < (fst v)   = bst_lookup k l        
                                | k > (fst v)   = bst_lookup k r      

-- Rotación simple a derecha
singleR :: BTree32 k a -> BTree32 k a
singleR Nil = Nil
singleR (Node a sx vx (Node b sy vy c)) = (Node (Node a (size a + size b + 1) vx b) sx vy c)

-- Rotación simple a izquierda
singleL :: BTree32 k a -> BTree32 k a
singleL Nil = Nil
singleL (Node (Node a sx vx b) sy vy c) = (Node a sy vx (Node b (size b + size c + 1) vy c))

-- Rotación doble a derecha
doubleR :: BTree32 k a -> BTree32 k a
doubleR Nil = Nil
doubleR (Node (Node a sx vx b) sy vy (Node c sz vz d)) = 
                        (Node a sy vx (Node (Node b (size b + size c + 1) vy c) (size b + size c + size d + 1) vz d))

-- Rotación doble a izquierda
doubleL :: BTree32 k a -> BTree32 k a
doubleL Nil = Nil
doubleL (Node a sx vx (Node (Node b sy vy c) sz vz d)) = 
                        (Node (Node a (size a + size b + 1) vx b) sx vy (Node c (size c + size d + 1) vz d))


-- Crea un BTree32 balanceado a partir de un elemento y dos subárboles
balance :: Ord k => BTree32 k a -> (k, a) -> BTree32 k a -> BTree32 k a
balance Nil v Nil = Node Nil 1 v Nil

balance Nil v@(k,a) r@(Node _ sr vr _)      | k < (fst vr)      = (Node Nil (sr+1) v r)
                                            | k > (fst vr)      = (Node r (sr+1) v Nil)
                                            | otherwise         = r
                                            
balance l@(Node _ sl vl _) v@(k,a) Nil      | k < (fst vl)      = (Node Nil (sl+1) v l)
                                            | k > (fst vl)      = (Node l (sl+1) v Nil)
                                            | otherwise         = l

balance l@(Node ll sl vl rl) v r@(Node lr sr vr rr)     | sl + sr <= 1      = Node l (sl+sr+1) v r
                                                        | sr > 3 * sl       = if size lr < 2 * size rr
                                                                                then let (Node l' s' v' r') = singleL(Node l (sl+sr+1) v r)
                                                                                        in balance l' v' r'
                                                                                else let (Node l' s' v' r') = doubleL(Node l (sl+sr+1) v r)
                                                                                        in balance l' v' r'
                                                        | sl > 3 * sr       = if size ll < 2 * size rl
                                                                                then let (Node l' s' v' r') = singleR(Node l (sl+sr+1) v r)
                                                                                        in balance l' v' r'
                                                                                else let (Node l' s' v' r') = doubleR(Node l (sl+sr+1) v r)
                                                                                        in balance l' v' r'
                                                        | otherwise         = Node l (sl+sr+1) v r


-- Verifica que un BTree32 esté balanceado
isBalanced :: Ord k => BTree32 k a -> Bool
isBalanced Nil = True
isBalanced (Node Nil s v Nil) = True
isBalanced (Node l s v r) 	| size r == 1 && size l == 0    = True
                            | size r == 0 && size l == 1    = True
                            | size r > (3 * size l)        	= False
                            | size l > (3 * size r)        	= False
                            | otherwise                     = isBalanced l && isBalanced r


-- Inserta un elemento en un BTree32 manteniendo el invariante
insert :: Ord k => (k, a) -> BTree32 k a -> BTree32 k a
insert x Nil = Node Nil 1 x Nil
insert x@(k, a) t@(Node l s v r)	| k < fst v	= balance (insert x l) v r
									| k > fst v	= balance l v (insert x r)
									| otherwise = t

-- Elimina la raiz de un BTree32 manteniendo el invariante
delRoot :: Ord k => BTree32 k a -> BTree32 k a
delRoot Nil = Nil
delRoot (Node Nil s v Nil) = Nil
delRoot (Node l s v r)  | size l < size r   = let (Node l' s' v' r') = r in balance l  v' (delRoot r)
                        | otherwise         = let (Node l' s' v' r') = l in balance (delRoot l) v' r

-- Elimina un elemento en un BTree32 manteniendo el invariante
delete :: Ord k => k -> BTree32 k a -> BTree32 k a
delete _ Nil = Nil
delete x t@(Node l s v r)   | x == (fst v)  = delRoot t  
                            | x < (fst v)   = balance (delete x l) v r
                            | x > (fst v)   = balance l v (delete x r)


-- Creo un diccionario de pruebas
d1 = vacia :: BTree32 Int Char
d2 = insert (1 , 'c') d1
d3 = insert (12 , 'f') d2
d4 = insert (8 , 'e') d3
d5 = insert (15 , 'h') d4
d6 = insert (7 , 'i') d5
d7 = insert (43 , 'j') d6
d8 = insert (18 , 'k') d7
d9 = insert (2 , 'l') d8
d10 = insert (13 , 'm') d9
d11 = insert (25 , 'n') d10
d12 = insert (34 , 'o') d11
d13= insert (26 , 'p') d12
d14 = insert (45 , 'q') d13
d15 = insert (14 , 'r') d14
d = insert (46 , 'r') d15
