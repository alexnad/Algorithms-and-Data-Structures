
data Tree a = Empty | Node Int a (Tree a) (Tree a) deriving (Show)


key :: Tree a -> a
key (Node _ k _ _ ) = k


priority :: Tree a -> Int
priority (Node p _ _ _) = p


left :: Tree a -> Tree a
left (Node _ _ l _) = l


right :: Tree a -> Tree a
right (Node _ _ _ r) = r


find :: (Eq a, Ord a)  => Tree a -> a -> Bool
find Empty element = False
find (Node p key left right) element
    | element == key = True
    | element > key = find right element
    | element < key = find left element


rotateLeft :: Tree a -> Tree a
rotateLeft node = Node sonP sonKey sonLeft parent
    where
        son = (left parent)
        sonP = priority son
        sonKey = key son
        sonLeft = (left son)
        parent = Node (priority node) (key node) (right son) (right node)


rotateRight :: Tree a -> Tree a
rotateRight node = Node sonP sonKey parent sonRight
    where
        son = (left parent)
        sonP = priority son
        sonKey = key son
        sonRight = (right son)
        parent = Node (priority node) (key node) (left node) sonRight


insert :: (Ord a, Eq a) => Tree a -> a -> Int -> Tree a
insert root value p
    | value < (key root) =
        if p < (priority root)
         then
            rotateLeft insertLeft
         else
            insertLeft
    | value > (key root) = 
        if p < (priority root)
         then
            rotateLeft insertRight
         else
            insertRight
    where
        insertLeft = Node (priority root) (key root) (insert (left root) value p) (right root)
        insertRight = Node (priority root) (key root) (left root) (insert (right root) value p)