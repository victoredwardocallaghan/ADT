module BST where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

leaf :: a -> Tree a
leaf x = Node x Empty Empty

pp :: Show a => Tree a -> IO ()
pp = mapM_ putStrLn . treeIndent
  where
    treeIndent Empty          = ["-- /-"]
    treeIndent (Node v lb rb) =
      ["--" ++ show v] ++
      map ("  |" ++) ls ++
      ("  `" ++ r) : map ("  " ++) rs
      where
        (r:rs) = treeIndent rb
        ls     = treeIndent lb

size :: Num a => Tree b -> a
size Empty        = 0
size (Node _ l r) = 1 + size l + size r

toList :: Tree a -> [a]
toList Empty        = []
toList (Node x l r) = [x] ++ toList l ++ toList r

fromList :: Ord a => [a] -> Tree a
fromList []     = Empty
fromList (x:xs) = Node x (fromList lefts) (fromList rights)
                  where p       = (<= x)
                        lefts   = takeWhile p xs
                        rights  = dropWhile p xs
