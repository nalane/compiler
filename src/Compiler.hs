module Compiler (
    Compiler, tableCount, fmap, (>>=)
) where

data CompilerState = CompilerState {
    tableCount :: Int
}

data Compiler a = Compiler (CompilerState -> (CompilerState, a))

instance Functor Compiler where
    fmap f (Compiler m) = Compiler $ \s -> 
        let (newS, val) = m s
        in (newS, f val)
        
instance Applicative Compiler where
    pure v = Compiler $ \s -> (s, v)
    (<*>) (Compiler f) (Compiler v) = Compiler $ \s ->
        let (s1, func) = f s
            (s2, val) = v s1
        in (s2, func val)

instance Monad Compiler where
    (>>=) (Compiler m) f = Compiler $ \s ->
        let (newS, val) = m s
            (Compiler newM) = f val
        in newM newS
