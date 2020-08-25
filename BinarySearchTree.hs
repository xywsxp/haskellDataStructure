data BinarySearchTree a = Nil | Node a (BinarySearchTree a) (BinarySearchTree a) deriving (Show)

insert :: (Ord a) => BinarySearchTree a -> a -> BinarySearchTree a

insert Nil a = Node a Nil Nil

insert (Node root left right) x 
    | x > root = Node root left (insert right x)
    | otherwise = Node root (insert left x) right

getSortedList :: (Ord a) => BinarySearchTree a -> [a]

getSortedList Nil = []
getSortedList (Node root left right) = 
    getSortedList left ++ [root] ++ getSortedList right