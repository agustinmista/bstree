class Diccionario t where
    vacia       :: Ord k => t k v
    insertar    :: Ord k => (k, v) -> t k v -> t k v
    eliminar    :: Ord k => k -> t k v -> t k v
    buscar      :: Ord k => k -> t k v -> Maybe v


data BTree32 k a = Nil | Node (BTree32 k a) Int (k, a) (BTree32 k a)

instance (Show k, Show a) => Show (BTree32 k a) where
  show Nil = "Nil"
  show (Node l s v r) = "(" ++ show l ++ " " ++ show s ++
                        " " ++ show v ++ " " ++ show r ++ ")"


size :: BTree32 k a -> Int
size Nil = 0
size (Node _ s _ _) = s

bst_lookup :: Ord k => k -> BTree32 k a -> Maybe a
bst_lookup _ Nil = Nothing                                              
bst_lookup k (Node l s v r)     | k == (fst v)  = Just (snd v)          
                                | k < (fst v)   = bst_lookup k l        
                                | k > (fst v)   = bst_lookup k r      


singleL :: BTree32 k a -> BTree32 k a
singleL Nil = Nil
singleL (Node a sx vx (Node b sy vy c)) = (Node (Node a sx vx b) sy vy c)

singleR :: BTree32 k a -> BTree32 k a
singleR Nil = Nil
singleR (Node (Node a sx vx b) sy vy c) = (Node a sx vx (Node b sy vy c))

doubleL :: BTree32 k a -> BTree32 k a
doubleL Nil = Nil
doubleL (Node (Node a sx vx b) sy vy (Node c sz vz d)) = 
                        (Node a sx vx (Node (Node b sy vy c) sz vz d))

doubleR :: BTree32 k a -> BTree32 k a
doubleR Nil = Nil
doubleR (Node a sx vx (Node (Node b sy vy c) sz vz d)) = 
                        (Node (Node a sx vx b) sy vy (Node c sz vz d))

balance :: BTree32 k a -> (k, a) -> BTree32 k a -> BTree32 k a
balance = undefined



insert :: Ord k => (k, a) -> BTree32 k a -> BTree32 k a
insert = undefined

delRoot :: Ord k => BTree32 k a -> BTree32 k a
delRoot = undefined

delete :: Ord k => k -> BTree32 k a -> BTree32 k a
delete = undefined
