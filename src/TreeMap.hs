module TreeMap (Tree (..), put, get, size, removeKey, containsKey, mapFromList, filterMap, foldlMap, foldrMap, mapMap, mergeMaps) where

data Tree k v = Nil | Node {keyvalue :: (k, v), leftchild :: Tree k v, rightchild :: Tree k v} deriving (Show, Eq)

--- PUT ---

put :: (Ord k) => k -> v -> Tree k v -> Tree k v
put k v Nil = Node (k, v) Nil Nil
put k v (Node kv@(k', _) left right)
  | k == k' = Node (k, v) left right
  | k < k' = Node kv (put k v left) right
  | otherwise = Node kv left (put k v right)

putKV :: (Ord k) => (k, v) -> Tree k v -> Tree k v
putKV (k, v) = put k v

putIf :: (Ord k) => (k -> Bool) -> k -> v -> Tree k v -> Tree k v
putIf predi k v tree = if predi k then put k v tree else tree

--- GET ---

get :: (Ord k) => k -> Tree k v -> Maybe v
get _ Nil = Nothing
get k (Node (k', v') left right)
  | k == k' = Just v'
  | k < k' = get k left
  | otherwise = get k right

--- SIZE ---

size :: Tree k v -> Int
size Nil = 0
size (Node _ l r) = 1 + size l + size r

--- REMOVE ---

-- If the key to remove is less/greater than the current node -> recursively call left/right node
-- If the key to remove is equal to the current node -> call removeRoot
removeKey :: (Ord k) => k -> Tree k v -> Tree k v
removeKey _ Nil = Nil
removeKey k n@(Node kv@(k', _) left right)
  | k == k' = removeRoot n
  | k < k' = Node kv (removeKey k left) right
  | otherwise = Node kv left (removeKey k right)

-- In case node has no successors -> just remove
-- In case node has one successor -> swap with the successor and remove
-- In case node has two successors -> swap with the inorder (leftmost in the right child) successor and remove
removeRoot :: Tree k v -> Tree k v
removeRoot Nil = Nil
removeRoot (Node _ Nil Nil) = Nil
removeRoot (Node _ left Nil) = left
removeRoot (Node _ Nil right) = right
removeRoot (Node _ left right) = Node kv' left right'
  where
    kv' = keyvalue $ leftmost right
    right' = removeLeftmost right

leftmost :: Tree k v -> Tree k v
leftmost Nil = Nil
leftmost n@(Node _ Nil _) = n
leftmost (Node _ left _) = leftmost left

removeLeftmost :: Tree k v -> Tree k v
removeLeftmost Nil = Nil
removeLeftmost (Node _ Nil right) = right
removeLeftmost (Node kv left right) = Node kv (removeLeftmost left) right

--- CONTAINS KEY ---

containsKey :: (Ord k) => k -> Tree k v -> Bool
containsKey k node = case get k node of
  Nothing -> False
  _ -> True

--- MAKE MAP FROM LIST OF PAIRS ---

mapFromList :: (Ord k) => [(k, v)] -> Tree k v
mapFromList = foldr putPair Nil
  where
    putPair (k, v) = put k v

--- FILTER ---

filterMap :: (Ord k) => (k -> Bool) -> Tree k v -> Tree k v
filterMap predi tree = filterHelper predi tree Nil
  where
    filterHelper _ Nil new = new
    filterHelper predi' (Node (k, v) left right) new =
      let filterLeft = filterHelper predi' left (putIf predi' k v new)
       in filterHelper predi' right filterLeft

--- FOLDS ---

foldlMap :: (Ord k) => (b -> (k, v) -> b) -> b -> Tree k v -> b
foldlMap _ ini Nil = ini
foldlMap f ini (Node kv left right) =
  let leftResult = foldlMap f (f ini kv) left
   in foldlMap f leftResult right

foldrMap :: (Ord k) => ((k, v) -> b -> b) -> b -> Tree k v -> b
foldrMap _ ini Nil = ini
foldrMap f ini (Node kv left right) =
  let rightResult = kv `f` foldrMap f ini right
   in foldrMap f rightResult left

--- Map ---

mapMap :: (Ord k, Ord k0) => ((k, v) -> (k0, v0)) -> Tree k v -> Tree k0 v0
mapMap mapper node = helperMap mapper node Nil

helperMap :: (Ord k, Ord k0) => ((k, v) -> (k0, v0)) -> Tree k v -> Tree k0 v0 -> Tree k0 v0
helperMap _ Nil new = new
helperMap mapper (Node kv left right) new =
  let leftResult = helperMap mapper left new
   in helperMap mapper right (putKV (mapper kv) leftResult)

--- Merge ---

mergeMaps :: (Ord k) => Tree k v -> Tree k v -> Tree k v
mergeMaps Nil m2 = m2
mergeMaps m1 Nil = m1
mergeMaps n1 (Node kv2 left2 right2) =
  let leftResult = mergeMaps (putKV kv2 n1) left2
   in mergeMaps leftResult right2
