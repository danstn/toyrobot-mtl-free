{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module DataTypesALaCarte where

import Control.Monad.Free
import Data.Functor.Coyoneda

infixr :+:

data (f :+: g) a = Inl (f a)
                 | Inr (g a)
                 deriving (Functor)

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance Functor f => f :<: Coyoneda f where
  inj = liftCoyoneda

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

inject :: (MonadFree f m, g :<: f) => g (m a) -> (m a)
inject = wrap . inj

class (Functor f, Monad m) => Run f m where
  runAlgebra :: f (m a) -> m a

instance (Run f m, Run g m, Monad m) => Run (f :+: g) m where
  runAlgebra (Inl r) = runAlgebra r
  runAlgebra (Inr r) = runAlgebra r
