import Debug.Trace

class Diccionario t where
    vacia       :: Ord k => t k v
    insertar    :: Ord k => (k, v) -> t k v -> t k v
    eliminar    :: Ord k => k -> t k v -> t k v
    buscar      :: Ord k => k -> t k v -> Maybe v


data BTree32 k a = Nil | Node (BTree32 k a) Int (k, a) (BTree32 k a)

------------------------------------------------------------------------
indent :: [String] -> [String]
indent = map ("        "++)

layoutTree :: (Show k, Show a) => BTree32 k a -> [String]
layoutTree Nil = [] 
layoutTree (Node l s v r) 
         = indent (layoutTree r) ++ [show (fst v) ++ "->" ++ show (snd v), "  "++show s] ++ indent (layoutTree l)
         
prettyTree :: (Show k, Show a) => BTree32 k a -> String
prettyTree = unlines.layoutTree

instance (Show k, Show a) => Show (BTree32 k a) where
  show x =  prettyTree x
------------------------------------------------------------------------


size :: BTree32 k a -> Int
size Nil = 0
size (Node _ s _ _) = s

bst_lookup :: Ord k => k -> BTree32 k a -> Maybe a
bst_lookup _ Nil = Nothing                                              
bst_lookup k (Node l s v r)     | k == (fst v)  = Just (snd v)          
                                | k < (fst v)   = bst_lookup k l        
                                | k > (fst v)   = bst_lookup k r      


singleR :: BTree32 k a -> BTree32 k a
singleR Nil = Nil
singleR (Node a sx vx (Node b sy vy c)) = (Node (Node a (size a + size b + 1) vx b) sx vy c)


singleL :: BTree32 k a -> BTree32 k a
singleL Nil = Nil
singleL (Node (Node a sx vx b) sy vy c) = (Node a sy vx (Node b (size b + size c + 1) vy c))


doubleR :: BTree32 k a -> BTree32 k a
doubleR Nil = Nil
doubleR (Node (Node a sx vx b) sy vy (Node c sz vz d)) = 
                        (Node a sy vx (Node (Node b (size b + size c + 1) vy c) (size b + size c + size d + 1) vz d))


doubleL :: BTree32 k a -> BTree32 k a
doubleL Nil = Nil
doubleL (Node a sx vx (Node (Node b sy vy c) sz vz d)) = 
                        (Node (Node a (size a + size b + 1) vx b) sx vy (Node c (size c + size d + 1) vz d))


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


isBalanced :: Ord k => BTree32 k a -> Bool
isBalanced Nil = True
isBalanced (Node Nil s v Nil) = True
isBalanced (Node l s v r) 	| size r == 1 && size l == 0    = True
                            | size r == 0 && size l == 1    = True
                            | size r > (3 * size l)        	= False
                            | size l > (3 * size r)        	= False
                            | otherwise                     = isBalanced l && isBalanced r

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

------------------------------------------------------------------------

t1 = insert (5,'a') Nil
t2 = insert (1 , 'c') t1
t3 = insert (12 , 'f') t2
t4 = insert (8 , 'e') t3
t5 = insert (15 , 'h') t4
t6 = insert (7 , 'i') t5
t7 = insert (43 , 'j') t6
t8 = insert (18 , 'k') t7
t9 = insert (2 , 'l') t8
t10 = insert (13 , 'm') t9
t11 = insert (25 , 'n') t10
t12 = insert (34 , 'o') t11
t13= insert (26 , 'p') t12
t14 = insert (45 , 'q') t13
t15 = insert (14 , 'r') t14
t16 = insert (46 , 'r') t15
