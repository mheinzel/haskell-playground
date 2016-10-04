{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeOperators #-}

-- First, a simple Reader Monad
newtype Reader r a = Reader { runReader :: (r -> a) }

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure = Reader . const
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    Reader f <*> Reader g = Reader $ \x -> f x (g x)

instance Monad (Reader r) where
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    m >>= k = Reader $ \r -> runReader (k (runReader m r)) r

ask :: Reader r r
ask = Reader id


-- Now, a function type!
-- You can:
--      lift regular functions to it
--      apply it using # (instead of whitespace)
newtype (-->) a b = Lift { (#) :: (a -> b) }
infixr 0 -->
infixl 9 #

lift :: (a -> b) -> a --> b
lift = Lift

lift2 :: (a -> b -> c) -> a --> b --> c
lift2 = lift . (lift .)

lift3 :: (a -> b -> c -> d) -> a --> b --> c --> d
lift3 = lift . (lift2 .)

instance Functor ((-->) a) where
    fmap :: (b -> c) -> (a --> b) -> a --> c
    fmap f g = Lift $ f . (g #)

instance Applicative ((-->) a) where
    pure :: b -> a --> b
    pure = lift . const
    (<*>) :: (a --> b -> c) -> (a --> b) -> a --> c
    m <*> n = Lift $ \a -> (m # a) (n # a)

instance Monad ((-->) a) where
    -- Don't these mixed arrows look nice?
    (>>=) :: (a --> b) -> (b -> a --> c) -> a --> c
    m >>= k = Lift $ \a -> k (m # a) # a

max' :: Ord a => a --> a --> a
max' = lift2 max

n = max' # 2 # 3

