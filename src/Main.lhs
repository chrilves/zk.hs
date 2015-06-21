> module Main where

> import ZK.IdxTree

class MonadPlus m => Search m where
   local   :: m a -> m a
   onEmpty :: m a -> (a -> m b) -> m b -> m b     

> main :: IO ()
> main = putStrLn "Hello"
