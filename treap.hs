
data Tree k = Empty | Node Int k (Tree k) (Tree k) deriving (Show)


key :: Tree -> a
key (Node _ k _ _ ) = k


priority :: Tree -> a
priority (Node p _ _ _) = p

left :: Tree -> a
left (Node _ _ l _) = l


right :: Tree -> a
right (Node _ _ _ r) = r


find :: (Eq a, Ord a)  => Tree k a -> a -> Bool
find Empty element = False
find (Node p key left right) element
    | element == key = True
    | element > key = find right element
    | element < key = find left element


rotateLeft node = Node sonP sonKey sonLeft parent
    where
        son = (left parent)
        sonP = priority son
        sonKey = key son
        sonLeft = (left son)
        parent = Node (priority node) (key node) (right son) (right node)


rotateLeft node = Node sonP sonKey parent sonRight
    where
        son = (left parent)
        sonP = priority son
        sonKey = key son
        sonRight = (right son)
        parent = Node (priority node) (key node) (left node) sonRight


