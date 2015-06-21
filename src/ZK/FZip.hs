module ZK.FZip where

class (Functor f) => FZip f where
  unfzip :: f (a,b) -> (f a, f b)
  fzip   :: f a -> f b -> f (a,b)