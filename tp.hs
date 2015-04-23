import Data.Tree hiding (Tree, Node, Nil)

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
         = indent (layoutTree r) ++ ["S: "++show s, show (fst v) ++ "->" ++ show (snd v), ""] ++ indent (layoutTree l)
         
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
singleR t@(Node Nil s v Nil) = t
singleR (Node a sx vx (Node b sy vy c)) = (Node (Node a (size a + size b + 1) vx b) sx vy c)

singleL :: BTree32 k a -> BTree32 k a
singleL Nil = Nil
singleL (Node (Node a sx vx b) sy vy c) = (Node a sy vx (Node b (size b + size c + 1) vy c))

doubleR :: BTree32 k a -> BTree32 k a
doubleR Nil = Nil
doubleR (Node (Node a sx vx b) sy vy (Node c sz vz d)) = 
                        (Node a sy vx (Node (Node b (size b + size c + 1) vy c) (size b + size c + size d + 2) vz d))

doubleL :: BTree32 k a -> BTree32 k a
doubleL Nil = Nil
doubleL (Node a sx vx (Node (Node b sy vy c) sz vz d)) = 
                        (Node (Node a (size a + size b + 1) vx b) sx vy (Node c (size c + size d + 1) vz d))

balance :: BTree32 k a -> (k, a) -> BTree32 k a -> BTree32 k a
balance Nil v Nil = Node Nil 1 v Nil
balance Nil v r@(Node lr sr vr rr) = 
balance l@(Node ll sl vl rl) v Nil = let (Node l' s' v' r') = singleL(Node l (sl+1) v Nil)
                                        in balance l' v' r'
balance l@(Node ll sl vl rl) v r@(Node lr sr vr rr)     | sr > 3 * sl        =  if size lr < 2 * size rr
                                                                                then let (Node l' s' v' r') = singleL(Node l (sl+sr+1) v r)
                                                                                        in balance l' v' r'
                                                                                else let (Node l' s' v' r') = doubleL(Node l (sl+sr+1) v r)
                                                                                        in balance l' v' r'
                                                        | sl > 3 * sr        =  if size ll < 2 * size rl
                                                                                then let (Node l' s' v' r') = singleR(Node l (sl+sr+1) v r)
                                                                                        in balance l' v' r'
                                                                                else let (Node l' s' v' r') = doubleR(Node l (sl+sr+1) v r)
                                                                                        in balance l' v' r'
                                                        | otherwise          =  Node l (sl+sr+1) v r


insert :: Ord k => (k, a) -> BTree32 k a -> BTree32 k a
insert x Nil = Node Nil 1 x Nil
insert x@(k, a) t@(Node l s v r)	| k < fst v	= balance (insert x l) v r
									| k > fst v	= balance l v (insert x r)
									| otherwise = t

delRoot :: Ord k => BTree32 k a -> BTree32 k a
delRoot = undefined

delete :: Ord k => k -> BTree32 k a -> BTree32 k a
delete = undefined



j = (Node Nil 1 (1,'j') Nil)
h = (Node Nil 1 (2,'h') Nil)
i = (Node Nil 1 (3,'i') Nil)
d = (Node Nil 1 (4,'d') Nil)

b = (Node d 2 (5,'b') Nil)
g = (Node j 2 (6,'g') Nil)
f = (Node i 2 (7,'f') Nil)
e = (Node g 4 (8,'e') h)
c = (Node e 7 (9,'c') f)

a = (Node b 10 (10,'a') c)




