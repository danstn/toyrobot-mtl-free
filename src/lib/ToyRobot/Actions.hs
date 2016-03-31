{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ToyRobot.Actions where

import           Control.Monad.Free
import           Control.Monad.State
import           Data.Functor.Coproduct (Coproduct(Coproduct), getCoproduct)

data InteractionF a = Ask String (String -> a)
                   | Tell String a
                   deriving Functor

data RobotF next = Place (Integer, Integer) next
                     | Report next
                     | Move next
                     | TurnLeft next
                     | TurnRight next
                     | Steer (String -> next)
                     | BadCommand next
                     | Done
                     deriving Functor

type FreeRobot a       = Free RobotF a
type FreeInteraction a = Free InteractionF a

-- Machinery
-- ----------------------------------------------------------------------------
infixr 6 |*|
(|*|) :: (Functor f, Functor g) =>
  (forall a. f a -> g a) -> (forall b. h b -> g b) -> Coproduct f h c -> g c
fg |*| hg = \x -> case getCoproduct x of
  Left f -> fg f
  Right h -> hg h

newtype Program a = Program { run :: Coproduct InteractionF RobotF a } deriving Functor

interactionC :: InteractionF a -> Program a
interactionC = Program . Coproduct . Left

robotC :: RobotF a -> Program a
robotC = Program . Coproduct . Right

magicFold :: (Functor f, Monad m) => (forall b. f b -> m b) -> Free f a -> m a
magicFold _ (Pure a) = return a
magicFold f (Free fa) = f fa >>= magicFold f

threadF :: (Functor f, Functor g) => (forall b. f b -> g b) -> Free f a -> Free g a
threadF _ (Pure x)  = Pure x
threadF t (Free fa) = Free(t $ fmap (threadF t) fa)


tell :: String -> Free Program ()
tell msg = threadF interactionC (liftF $ Tell msg ())

place :: (Integer, Integer) -> Free Program ()
place p = threadF robotC (liftF $ Place p ())

move :: Free Program ()
move = threadF robotC (liftF $ Move ())

report :: Free Program ()
report = threadF robotC (liftF $ Report ())

turnLeft :: Free Program ()
turnLeft = threadF robotC (liftF $ TurnLeft ())

turnRight :: Free Program ()
turnRight = threadF robotC (liftF $ TurnRight ())

steer :: Free Program String
steer = threadF robotC (liftF $ Steer id)

badCommand :: Free Program ()
badCommand = threadF robotC (liftF $ BadCommand ())

done :: Free Program ()
done = threadF robotC (liftF Done)





