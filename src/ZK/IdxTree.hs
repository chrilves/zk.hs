{-# LANGUAGE FlexibleContexts , UndecidableInstances #-}
module ZK.IdxTree where

import Prelude hiding (unzip,interact)

import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Applicative
import Control.Arrow
import ZK.FZip

--------------------------------------------------------------------------------------
--  Definition
--------------------------------------------------------------------------------------

data IdxTree n i f l = Leaf l
                     | Node { nodeValue :: n
                            , children  :: i -> f (IdxTree n i f l)
                            }
                            
instance (Show l, Show n) => Show (IdxTree n i f l) where
    show (Leaf l)   = "Leaf " ++ show l
    show (Node n _) = "Node " ++ show n ++ " _"
                            
instance Functor f => Functor (IdxTree n i f) where
  fmap h (Leaf a)   = Leaf $ h a
  fmap h (Node n c) = Node n $ (fmap $ fmap h) . c
  
instance Functor f => Applicative (IdxTree n i f) where
  pure l = Leaf l
  
  (Leaf h)   <*> t = fmap h t
  (Node n c) <*> t = Node n $ (fmap (flip (<*>) t)) . c
  
instance Functor f => Monad (IdxTree n i f) where
  return l = Leaf l
  
  (Leaf a)   >>= h = h a
  (Node n c) >>= h = Node n $ (fmap (flip (>>=) h)) . c
  
--------------------------------------------------------------------------------------
--  Functions
--------------------------------------------------------------------------------------

getLeaf :: Monad f => Kleisli f n i-> IdxTree n i f l -> f l
getLeaf select (Leaf l)   = return l
getLeaf select (Node n c) = runKleisli(select) n >>= c >>= getLeaf select

collide :: (FZip f, Monad f) => IdxTree n i f l1 -> IdxTree i n f l2 -> f (IdxTree n i f l1 , IdxTree i n f l2)
collide (Node n ci) (Node i cn) = do (t1, t2) <- fzip (ci i) (cn n) 
                                     collide t1 t2
collide   t1            t2      = return (t1,t2)

liftIdxTree :: (Monad f, Functor f) => (a -> f b) -> IdxTree n i f a -> f (IdxTree n i f b)
liftIdxTree h (Leaf a)   = fmap (Leaf) (h a) 
liftIdxTree h (Node n c) = return $ Node n $ c >=> liftIdxTree h

focus :: (Monad f) => IdxTree n i f l -> (n -> IdxTree a b f i) -> f (IdxTree a b f l)
focus (Leaf l)    _ = return $ Leaf l
focus (Node n ci) h = aux $ h n
   where aux (Leaf i)    = ci i >>= (flip focus h)
         aux (Node a ca) = return $ Node a (ca >=> aux)


--------------------------------------------------------------------------------------
--  ContT Mapping
--------------------------------------------------------------------------------------


type IdxTreeContT n i m l a = ContT (IdxTree n i m l) m a


leaf :: Monad f => l -> IdxTreeContT n i f l a
leaf l = ContT (\k -> return $ Leaf l)

node :: Monad f => n -> IdxTreeContT n i f l i
node n = ContT (\k -> return $ Node n k)

runIdxTreeContT :: Monad f => IdxTreeContT n i f l l -> f (IdxTree n i f l)
runIdxTreeContT x = runContT (x >>= leaf) return

--------------------------------------------------------------------------------------
--  IdxTreeT
--------------------------------------------------------------------------------------

newtype IdxTreeT n i f l = IdxTreeT { runIdxTreeT :: f (IdxTree n i f l) }                            

instance Show (f (IdxTree n i f l)) => Show (IdxTreeT n i f l) where
  show = ("IdxTreeT " ++) . show . runIdxTreeT  

instance MonadTrans (IdxTreeT n i) where
  lift ml = IdxTreeT $ ml >>= (\l -> return $ Leaf l) 
    
instance Functor f => Functor (IdxTreeT n i f) where
  fmap h t = IdxTreeT $ fmap (fmap h) (runIdxTreeT t)

instance Applicative f => Applicative (IdxTreeT n i f) where
  pure l = IdxTreeT $ pure (Leaf l)
  
  h <*> t = IdxTreeT $ (pure (<*>)) <*> (runIdxTreeT h) <*> (runIdxTreeT t)
  
instance Monad f => Monad (IdxTreeT n i f) where
  return l = IdxTreeT $ return $ Leaf l
  
  m >>= h = IdxTreeT $ runIdxTreeT m >>= aux
      where g = runIdxTreeT . h
      
            aux (Leaf l  ) = g l
            aux (Node n c) = return $ Node n $ c >=> aux
            
--------------------------------------------------------------------------------------
--  Functions
--------------------------------------------------------------------------------------

collideT :: (FZip f, Monad f) => IdxTreeT n i f l1 -> IdxTreeT i n f l2 -> (IdxTreeT n i f l1 , IdxTreeT i n f l2)
collideT x y = let (l,r) = unfzip $ do t1 <-runIdxTreeT x
                                       t2 <-runIdxTreeT y
                                       collide t1 t2
               in (IdxTreeT l, IdxTreeT r)
               
               
liftIdxTreeT :: (Monad f, Functor f) => (a -> f b) -> IdxTreeT n i f a -> IdxTreeT n i f b
liftIdxTreeT h t = IdxTreeT $ runIdxTreeT t >>= aux
  where aux (Leaf a)   = fmap (Leaf) (h a) 
        aux (Node n c) = return $ Node n $ c >=> aux


focusT :: (Monad f) => IdxTreeT n i f l -> (n -> IdxTreeT a b f i) -> IdxTreeT a b f l
focusT t h = IdxTreeT $ runIdxTreeT t >>= aux
  where aux (Leaf l)    = return $ Leaf l
        aux (Node n ci) = (runIdxTreeT $ h n) >>= aux2 
          where aux2 (Leaf i)    = ci i >>= aux
                aux2 (Node a ca) = return $ Node a $ ca >=> aux2 
               