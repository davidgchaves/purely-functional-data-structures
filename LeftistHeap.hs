module LeftistHeap where
-- Leftist heaps
--  heap-ordered binary trees that satisfy the leftist property:
--  "the rank of any left child is at least as large
--   as the rank of its right sibling"

  import Heap

  data LeftistHeap a = E
                     | T Int a (LeftistHeap a) (LeftistHeap a)

  -- The rank of a node is defined to be the length of its right spine
  -- (the rightmost path from the node in question to an empty node)
  rank             :: LeftistHeap lh -> Int
  rank E           = 0
  rank (T r _ _ _) = r

  -- makeT: calculates the rank of a T node
  --        and swaps its children if necessary
  makeT                :: lh -> LeftistHeap lh -> LeftistHeap lh -> LeftistHeap lh
  makeT x a b
    | rank a >= rank b = T (rank b+1) x a b
    | otherwise        = T (rank a+1) x b a

  instance Heap LeftistHeap where
--  empty :: (Heap h, Ord a) => h a
    empty = E

--  isEmpty   :: (Heap h, Ord a) => h a -> Bool
    isEmpty E = True
    isEmpty _ = False

    -- insert: creates a new singleton tree and merges it with the existing heap
--  insert     :: (Heap h, Ord a) => a -> h a -> h a
    insert x h = merge (T 1 x E E) h

    -- merge: two leftist heaps can be merged by:
    --        - merging their right spines (as you would merge two sorted lists), and then
    --        - swapping the children of nodes along this path to restore the leftist property
--  merge           :: (Heap h, Ord a) => h a -> h a -> h a
    merge h E       = h
    merge E h       = h
    merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
        | x <= y    = makeT x a1 (merge b1 h2)
        | otherwise = makeT y a2 (merge h1 b2)

    -- findMin: returns the root element
--  findMin             :: (Heap h, Ord a) => h a -> a
    findMin E           = error "empty heap"
    findMin (T _ x _ _) = x

    -- delMin: discards the root element and merges its children
--  delMin             :: (Heap h, Ord a) => h a -> h a
    delMin E           = error "empty heap"
    delMin (T _ _ a b) = merge a b
