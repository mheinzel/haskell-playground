{-# LANGUAGE FlexibleInstances #-}
module Epsilon where

-- testing if predicates on infinite lists of Bools are
-- -> satisfiable
-- -> a tautology
-- -> equal to each other
-- the predicates have to execute in finite time on every input

type Predicate = [Bool] -> Bool

instance Eq Predicate where
    p == q = tautology $ \xs -> p xs == q xs

tautology :: Predicate -> Bool
tautology p = p $ searcher (not . p)

find :: Predicate -> Maybe [Bool]
find p = if p xs then Just xs else Nothing
  where
    xs = searcher p

-- the core of the program:
-- returns a list satisfying the predicate, if any exists
-- returns any infinite list otherwise
searcher :: ([Bool] -> Bool) -> [Bool]
searcher p = b : searcher (p . (b :))
  where
    -- the b only has to be evaluated for the elements that are actually tested
    -- True, if we can find a valid list with True as first element
    -- False otherwise (either there is a valid list starting with False, or we don't care)
    b = p (True : searcher (p . (True :)))

--}

{-
searcher :: Predicate -> [Bool]
searcher p =
  if p (True : searcher (p . (True:)))
    then True  : searcher (p . (True  :))
    else False : searcher (p . (False :))
--}
