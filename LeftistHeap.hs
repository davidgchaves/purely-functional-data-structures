module LeftistHeap where
  import Heap

  data LeftistHeap a = E
                     | T Int a (LeftistHeap a) (LeftistHeap a)
