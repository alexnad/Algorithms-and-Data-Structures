import qualified Data.Set as Set
import qualified System.Random as R


data Tree a = Empty | Node Int a (Tree a) (Tree a) deriving (Show, Eq)


key :: Tree a -> a
key (Node _ k _ _ ) = k


priority :: Tree a -> Int
priority (Node p _  _ _) = p


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
rotateLeft parent = Node sonP sonKey sonLeft newParent
    where
        son = left parent
        sonP = priority son
        sonKey = key son
        sonLeft = left son
        newParent = Node (priority parent) (key parent) (right son) (right parent)


rotateRight :: Tree a -> Tree a
rotateRight parent = Node sonP sonKey newParent sonRight
    where
        son = (right parent)
        sonP = priority son
        sonKey = key son
        sonRight = (right son)
        newParent = Node (priority parent) (key parent) (left parent) sonRight


insert :: (Ord a, Eq a) => Tree a -> a -> Int -> Tree a
insert Empty value p = Node p value Empty Empty 
insert root value p
    | value == (key root) = root
    | value < (key root) =
        if p < (priority root)
         then
            rotateLeft insertLeft
         else
            insertLeft
    | value > (key root) =
        if p < (priority root)
         then
            rotateRight insertRight
         else
            insertRight
    where
        insertLeft = Node (priority root) (key root) (insert (left root) value p) (right root)
        insertRight = Node (priority root) (key root) (left root) (insert (right root) value p)


delete :: (Ord a, Eq a) => Tree a -> a -> Tree a
delete (Node p value Empty Empty) element =
    if(value == element) 
        then Empty 
        else Node p value Empty Empty
delete root element
    | element == (key root) =
        if leftSon == Empty || (rightSon /= Empty && priority leftSon > priority rightSon)
            then Node (priority rightSon) (key rightSon) leftSon rightDeletion
            else Node (priority leftSon) (key leftSon) leftDeletion rightSon
    | element < (key root) = delete (left root) element
    | element > (key root) = delete (right root) element
    where
        leftSon = (left root)
        rightSon = (right root)
        rightDeletion = delete (Node (priority root) (key root) (left rightSon) (right rightSon)) element
        leftDeletion = delete (Node (priority root) (key root) (left leftSon) (right leftSon)) element

 
split :: (Ord a, Eq a) => Tree a -> a -> (Tree a, Tree a)
split node value = (left t, right t)
    where
        t = insert node value (-1)


data UniqueKeyGenerator = UniqueKeyGenerator { generator :: [Int],
                                               used :: Set.Set Int}



createGen :: Int -> UniqueKeyGenerator
createGen seed = UniqueKeyGenerator (R.randomRs (0,1000000000) (R.mkStdGen seed)) Set.empty


tailGen :: UniqueKeyGenerator -> UniqueKeyGenerator
tailGen g = UniqueKeyGenerator (tail (generator g)) (used g) 


headGen :: UniqueKeyGenerator -> (Int, UniqueKeyGenerator)
headGen g = 
    if Set.member nextRand (used g)
        then
         (headGen (UniqueKeyGenerator (tail (generator g)) (used g)))
        else
         (nextRand, UniqueKeyGenerator (tail (generator g)) (Set.insert nextRand (used g)))
    where
        nextRand = head (generator g)


data Treap a = Treap {treap :: (Tree a), uniqueKeys :: UniqueKeyGenerator}


instance Show a => Show (Treap a) where
    show t = show (treap t)


createEmpty :: Int -> Treap a
createEmpty seed = Treap Empty (createGen seed)


tinsert :: (Ord a, Eq a) => Treap a -> a -> Treap a
tinsert t element = Treap (insert (treap t) element (next)) (gen)
    where
        (next, gen) = headGen (uniqueKeys t)


tfind :: (Ord a, Eq a) => Treap a -> a -> Bool
tfind t element = find (treap t) element


tdelete :: (Ord a, Eq a) => Treap a -> a -> Treap a
tdelete t element = Treap (delete (treap t) element) (uniqueKeys t)


tinsertList :: (Ord a, Eq a) => Treap a -> [a] -> Treap a
tinsertList t [] = t
tinsertList t list = tinsertList (tinsert t (head list)) (tail list)
