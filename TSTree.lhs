Ternary search trees

> module TSTree (TSTree (..), assocs, empty, insert, lookup) where
> 
> import Prelude hiding (lookup)
> import Data.Function (on)
> import qualified Data.Map as M

A ternary search tree is a space-efficient type of prefix tree. It is a map structure that associates strings with arbitrary values and allows for incremental searching. See https://en.wikipedia.org/wiki/Ternary_search_tree for more information.

The type TSTree represents ternary search trees. It is parameterized over the type variables k and v, denoting key and value types respectively. The actual "keys" of this structure are lists with elements of type k (called key sequences from here on), so e.g. a map from Strings to Ints has type TSTree Char Int. Some applications, such as auto-completion, may require no associated values and can use trees of the type TSTree k (), for some type k. Finally, a ternary search tree requires an ordering on its keys, so an Ord k instance must be defined. 

A TSTree k v may be Empty, or a Node with 5 fields:

1. The key of this node.
2. A Just x value if the sequence corresponding to this node is a member of the tree, otherwise Nothing.
3. The left child node, also known as "lo kid".
4. The middle child node, also known as "equal kid".
5. The right child node, also known as "hi kid".

> data TSTree k v
>     = Empty
>     | Node k (Maybe v) (TSTree k v) (TSTree k v) (TSTree k v)
>     deriving (Show)

The value empty is an alias for the Empty constructor of TSTree.

> empty :: TSTree k v
> empty = Empty

The function assocs returns a list of the key sequence / value associations of a TSTree. The empty tree has no associations. A Node may have zero or more of the following associations:

1. A value associated with the key sequence corresponding to this node.
2. The associations of the left and right children.
3. The associations of the middle child. The key sequence leading to this node is a prefix of these associations.

> assocs :: TSTree k v -> [([k], v)]
> assocs Empty                = []
> assocs (Node k mv lo eq hi) = assoc ++ lefts ++ middles ++ rights
>     where
>         assoc = case mv of
>             Nothing -> []
>             Just v  -> [([k], v)]
>         lefts = assocs lo
>         middles = map (\ (k', v') -> (k:k', v')) $ assocs eq
>         rights = assocs hi

Two ternary search trees are equal if they contain the same associations. Here, equality testing is implemented by taking the assocs of the two trees, converting them to Maps, and comparing these for equality using the predefined Eq instance for Map.

> instance (Ord k, Eq v) => Eq (TSTree k v) where
>     (==) = (==) `on` M.fromList . assocs

The function lookup returns the value associated with a key sequence in a TSTree. The empty tree has no associations for any key sequence, and no tree has an association for the empty key sequence. If both the key sequence and tree are non-empty, lookup proceeds as follows:

1. If the first element of the key sequence is less or greater than the key of this node, a lookup with the same key sequence is performed on the left or right child node, respectively.
2. Otherwise, the first element of the key sequence is equal to the key of this node. This situation has two sub-cases:

* If the tail (all elements but the first) of the key sequence is empty, the end of the sequence has been reached. The value associated with this node is returned, if one exists.
* If the tail of the key sequence is non-empty, a lookup with the tail is performed on the middle child node.

> lookup :: (Ord k) => [k] -> TSTree k v -> Maybe v
> lookup _      Empty                = Nothing
> lookup []     _                    = Nothing
> lookup (x:xs) (Node k mv lo eq hi)
>     | x < k                        = lookup (x:xs) lo
>     | x > k                        = lookup (x:xs) hi
>     | null xs                      = mv
>     | otherwise                    = lookup xs eq

The function insert associates a key sequence with a value in a TSTree. Inserting a value for the empty key sequence has no effect. Inserting an association into the empty tree builds nodes for each key in order, each node being the middle child of the previous one. For the last key, a leaf node with the given associated value is created. Insertion into a non-empty tree follows the logic of the lookup operation.

> insert :: (Ord k) => [k] -> v -> TSTree k v -> TSTree k v
> insert []        _  t                    = t
> insert [x]       v' Empty                = Node x (Just v') Empty Empty Empty
> insert (x:x1:xs) v' Empty                = Node x Nothing Empty (insert (x1:xs) v' Empty) Empty
> insert (x:xs)    v' (Node k mv lo eq hi)
>     | x < k                              = Node k mv (insert (x:xs) v' lo) eq hi
>     | x > k                              = Node k mv lo eq (insert (x:xs) v' hi)
>     | null xs                            = Node k (Just v') lo eq hi
>     | otherwise                          = Node k mv lo (insert xs v' eq) hi
