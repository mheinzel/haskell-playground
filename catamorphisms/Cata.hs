{-# LANGUAGE DeriveFunctor #-}

-- catamorphisms (or generic eliminators) are a generalization of a fold

-------------------------------------------------------------------------------
-- B - BASIC
-------------------------------------------------------------------------------

-- a simple binary tree
data TreeB a = LeafB a | NodeB (TreeB a) (TreeB a)

exampleB :: TreeB Int
exampleB =
    NodeB
        (NodeB
            (LeafB 3)
            (LeafB 9))
        (LeafB 7)

-- the eliminator, describing the recursion pattern
treeElimB :: (a -> b) -> (b -> b -> b) -> TreeB a -> b
treeElimB leaf node = go
  where
    go (LeafB a) = leaf a
    go (NodeB b b') = node (go b) (go b')

-- individual functions just need to plug in the functions
sizeB :: TreeB a -> Int
sizeB = treeElimB (const 1) (+)

maxB :: Ord a => TreeB a -> a
maxB = treeElimB id max

showB :: Show a => TreeB a -> String
showB = unlines . treeElimB leaf node
  where
    leaf a = ["Leaf " ++ show a]
    node l r = "Node" : map ("  " ++) (l ++ r)

-------------------------------------------------------------------------------
-- S - SHAPE
-------------------------------------------------------------------------------

-- the tree is now described as fixed point of its shape
newtype Fix f = In { unFix :: f (Fix f) }
data TreeShape a rec = Leaf a | Node rec rec
type TreeS a = Fix (TreeShape a)

exampleS =
    In $ Node
        (In $ Node
            (In $ Leaf 3)
            (In $ Leaf 9))
        (In $ Leaf 7)

-- Now we only need a single function from shape to result
-- this function forms an algebra
type Algebra f res = f res -> res

treeElimS :: Algebra (TreeShape a) b -> TreeS a -> b
treeElimS alg = go
  where
    go (In (Leaf a)) = alg (Leaf a)
    go (In (Node b b')) = alg (Node (go b) (go b'))


sizeAlg :: Algebra (TreeShape a) Int
sizeAlg (Leaf _) = 1
sizeAlg (Node l r) = l + r

maxAlg :: Ord a => Algebra (TreeShape a) a
maxAlg (Leaf a) = a
maxAlg (Node l r) = max l r

showAlg :: Show a => Algebra (TreeShape a) [String]
showAlg (Leaf a) = ["Leaf " ++ show a]
showAlg (Node l r) = "Node " : map ("  " ++) (l ++ r)

sizeS :: TreeS a -> Int
sizeS = treeElimS sizeAlg

maxS :: Ord a => TreeS a -> a
maxS = treeElimS maxAlg

showS :: Show a => TreeS a -> String
showS = unlines . treeElimS showAlg

-------------------------------------------------------------------------------
-- F - FUNCTOR
-------------------------------------------------------------------------------

-- instead of writing the eliminator by ourselves, we only need a shape functor
-- this instance could have been derived! (-> no work for us)
instance Functor (TreeShape a) where
    fmap f (Leaf a) = Leaf a
    fmap f (Node l r) = Node (f l) (f r)

-- we can get the eliminator generically for any shape functor!
genericElim :: Functor f => Algebra f b -> Fix f -> b
genericElim alg = alg . fmap (genericElim alg) . unFix

sizeF :: TreeS a -> Int
sizeF = genericElim sizeAlg

maxF :: Ord a => TreeS a -> a
maxF = genericElim maxAlg

showF :: Show a => TreeS a -> String
showF = unlines . genericElim showAlg


-------------------------------------------------------------------------------
-- LIST EXAMPLE
-------------------------------------------------------------------------------

data ListShape a rec = Nil | Cons a rec deriving Functor
type MyList a = Fix (ListShape a)

myLength :: MyList a -> Int
myLength = genericElim alg
  where
    alg Nil = 0
    alg (Cons _ res) = 1 + res

-- we can recreate the conventional fold function this way
myFoldr :: (a -> b -> b) -> b -> MyList a -> b
myFoldr f zero = genericElim alg
  where
    alg Nil = zero
    alg (Cons a res) = f a res

-- for convenience
nil = In Nil
x <: xs = In $ Cons x xs
infixr 5 <:

exampleList' :: MyList Int
exampleList' = 0 <: 9 <: 3 <: nil

