{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

import Control.Monad (ap)


-- INITIAL IDEA:
-- to decouple describing our program from executing the effectful operations,
-- whenever such an action would be necessary, we return which it is and
-- a continuation to call.
-- Now the caller has to execute all these actions for us and can decide
-- how to "interpret" it. We can define multiple interpreters e.g. for testing.
-- Only the logic of how these operations interact is encoded in the program.

data ConsoleProgram a = PrintForMe String (ConsoleProgram a)
                      | AskForMe String (String -> ConsoleProgram a)
                      | IHaveResult a

data User = User { userName :: String } deriving Show

-- our programs are now simply a recursive data structure, without any effects
greetUser :: ConsoleProgram User
greetUser =
    PrintForMe "Hello, I hope you are having a good day!"
        (AskForMe "What is your name?"
            (\name -> PrintForMe ("Nice to meet you, " ++ name)
                (IHaveResult (User name))))
-- the syntax is a bit cumbersome, we are going to get nice syntax later

-- we can interpret it using IO (call it with "interpretProg greetUser")
interpretProg :: ConsoleProgram a -> IO a
interpretProg (IHaveResult res) = return res
interpretProg (AskForMe question cont) = do
    putStrLn question
    response <- getLine
    interpretProg (cont response)
-- we don't even have to execute instructions one by one
-- we have access to the structure of the data type, which is very powerful
interpretProg (PrintForMe str1 (PrintForMe str2 cont)) = do
    putStrLn (str1 ++ "\n" ++ str2) -- execute two prints at once
    interpretProg cont
interpretProg (PrintForMe str cont) = do
    putStrLn str
    interpretProg cont

-- but we can also interpret it purely for testing
-- by looking up the response to questions purely and returning all printed strs
interpretProgPure :: (String -> String) -> ConsoleProgram a -> (a, [String])
-- we are done
interpretProgPure _ (IHaveResult res) = (res, [])
-- save str in list and continue interpreting the program
interpretProgPure answer (PrintForMe str cont) =
    fmap (str:) (interpretProgPure answer cont)
-- just look up the user input purely and continue intepreting with it
interpretProgPure answer (AskForMe question cont) =
    interpretProgPure answer (cont (answer question))

-- testing the program can be completely pure
testGreetUser :: (User, [String])
testGreetUser = interpretProgPure answer greetUser
  where
    answer "What is your name?" = "Richard"
    answer _ = "I don't know."


-- BUT HOW CAN WE GET NICE SYNTAX?
--
-- we can create a Monad instance for ConsoleProgram and get do-notation!
-- be we would have to do the same for other kinds of programs...
-- e.g. NetworkProgram, FilesystemProgram, ...
--
-- can we generalize the approach?
-- we could create a general data type, parametrized over the kind of program.

-- we can describe the kind/shape of the program type using a functor:
-- (ignore the constructor for returning a value for now)
data Console next = Print String next
                  | Ask String (String -> next)
                  deriving Functor
-- it is not recursive, but has a parameter for the continuation

-- but we can make Functors like this one recursive again:
data Fix f = Mu (f (Fix f))
-- Fix Console
-- = Mu (Console (Fix Console))
-- = Mu (Console (Mu (Console (Fix Console))))
-- = Mu (Console (Mu (Console (Mu (Console (Fix Console))))))
-- ...

-- but we also want to have a way to return values
-- (and be polymorphic over their type)
data Free f a = Roll (f (Free f a))
              | Return a


-- the type "Free Console a" is similar to our "ConsoleProgram a"
-- the corresponding program looks not much different than before,
-- just with a bit more wrapping.
greetUserFree :: Free Console User
greetUserFree =
    Roll (Print "Hello, I hope you are having a good day!"
        (Roll (Ask "What is your name?"
            (\name -> Roll (Print ("Nice to meet you, " ++ name)
                (Return (User name)))))))


-- now, let's implement the Monad instance for our Free f
-- note that f has to be a Functor

instance Functor f => Functor (Free f) where
    fmap :: (a -> b) -> Free f a -> Free f b
    fmap f (Return a) = Return (f a)
    fmap f (Roll functor) = Roll (fmap (fmap f) functor)

instance Functor f => Applicative (Free f) where
    pure :: a -> Free f a
    pure = Return
    (<*>) :: Free f (a -> b) -> Free f a -> Free f b
    (<*>) = ap -- <*> can be implemented generically for any monad

instance Functor f => Monad (Free f) where
    return = pure
    (>>=) :: Free f a -> (a -> Free f b) -> Free f b
    Return a >>= k = k a
    Roll functor >>= k = Roll (fmap (>>= k) functor)


-- also, we don't want to type Roll (Print ... all the time
-- let's write some functions for convenience!
printFree :: String -> Free Console ()
printFree str = Roll (Print str (return ()))
askFree :: String -> Free Console String
askFree question = Roll (Ask question return)

-- and see our program using do-notation!
greetUserFreeDo :: Free Console User
greetUserFreeDo = do
    printFree "Hello, I hope you are having a good day!"
    name <- askFree "What is your name?"
    printFree ("Nice to meet you, " ++ name)
    return (User name)
-- keep in mind, that it is still only a data structure
-- identical to greetUserFree

-- our interpreters stay almost the same
interpretConsoleIO :: Free Console a -> IO a
interpretConsoleIO (Return a) = return a
interpretConsoleIO (Roll (Print str cont)) = do
    putStrLn str
    interpretConsoleIO cont
interpretConsoleIO (Roll (Ask question cont)) = do
    putStrLn question
    response <- getLine
    interpretConsoleIO (cont response)

interpretConsolePure :: (String -> String) -> Free Console a -> (a, [String])
interpretConsolePure _ (Return a) = (a, [])
interpretConsolePure answer (Roll (Print str cont)) =
    fmap (str:) (interpretConsolePure answer cont)
interpretConsolePure answer (Roll (Ask question cont)) =
    interpretConsolePure answer (cont (answer question))

