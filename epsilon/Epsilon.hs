{-# LANGUAGE FlexibleInstances #-}

-- testing if predicates on infinite lists of Bools are
-- -> satisfiable
-- -> a tautology
-- -> equal to each other
-- the predicates have to execute in finite time on every input

type Predicate = [Bool] -> Bool

instance Eq Predicate where
    p == q = forall $ \xs -> p xs == q xs

forall :: Predicate -> Bool
forall p = p $ epsilon (not . p)

exists :: Predicate -> Maybe a
exists p = if p xs then Just xs else Nothing
  where
    xs = epsilon p

-- the core of the program:
-- returns a list satisfying the predicate, if any exists
-- returns any infinite list otherwise
epsilon :: Predicate -> [Bool]
epsilon p = b : epsilon (p . (b:))
  where
    -- the b only has to be evaluated for the elements that are actually tested
    -- True, if we can find a valid list with True as first element
    -- False otherwise (either there is a valid list starting with False, or we don't care)
    b = p (True : epsilon (p . (True:)))
