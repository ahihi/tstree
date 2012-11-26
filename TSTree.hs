module TSTree where

data TSTree k v
    = Empty
    | Node k (Maybe v) (TSTree k v) (TSTree k v) (TSTree k v)
    deriving (Eq, Show)

empty :: TSTree k v
empty = Empty

insert :: (Ord k) => [k] -> v -> TSTree k v -> TSTree k v
insert []     _  t                    = t
insert [x]    v' Empty                = Node x (Just v') Empty Empty Empty
insert [x]    v' (Node k mv lo eq hi)
    | x < k                           = Node k mv (insert [x] v' lo) eq hi
    | x > k                           = Node k mv lo eq (insert [x] v' hi)
    | otherwise                       = Node k (Just v') lo eq hi
insert (x:xs) v' Empty                = Node x Nothing Empty (insert xs v' Empty) Empty
insert (x:xs) v' (Node k mv lo eq hi)
    | x < k                           = Node k mv (insert (x:xs) v' lo) eq hi
    | x > k                           = Node k mv lo eq (insert (x:xs) v' hi)
    | otherwise                       = Node k mv lo (insert xs v' eq) hi

find :: (Ord k) => [k] -> TSTree k v -> Maybe v
find _      Empty                = Nothing
find []     _                    = Nothing
find (x:xs) (Node k mv lo eq hi)
    | x < k                      = find (x:xs) lo
    | x > k                      = find (x:xs) hi
    | null xs                    = mv
    | otherwise                  = find xs eq
