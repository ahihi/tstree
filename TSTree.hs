module TSTree where

data TSTree k v
    = Empty
    | Node k (Maybe v) (TSTree k v) (TSTree k v) (TSTree k v)
    deriving (Eq, Show)

empty :: TSTree k v
empty = Empty

insert :: (Ord k) => [k] -> v -> TSTree k v -> TSTree k v
insert []        _  t                    = t
insert [x]       v' Empty                = Node x (Just v') Empty Empty Empty
insert (x:x1:xs) v' Empty                = Node x Nothing Empty (insert (x1:xs) v' Empty) Empty
insert (x:xs)    v' (Node k mv lo eq hi)
    | x < k                              = Node k mv (insert (x:xs) v' lo) eq hi
    | x > k                              = Node k mv lo eq (insert (x:xs) v' hi)
    | otherwise = case xs of
        []                              -> Node k (Just v') lo eq hi
        _:_                             -> Node k mv lo (insert xs v' eq) hi

find :: (Ord k) => [k] -> TSTree k v -> Maybe v
find _      Empty                = Nothing
find []     _                    = Nothing
find (x:xs) (Node k mv lo eq hi)
    | x < k                      = find (x:xs) lo
    | x > k                      = find (x:xs) hi
    | null xs                    = mv
    | otherwise                  = find xs eq
