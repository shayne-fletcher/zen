import Control.Monad.State.Strict
import Data.Functor.Identity

data Counter = Counter{value :: Int, numberOfIncrements :: Int} deriving (Show)

incrementCounter :: Counter -> Identity (Int, Counter)
incrementCounter (Counter{value=oldValue, numberOfIncrements=oldNumberOfIncrements}) = 
  Identity (oldValue, Counter{value = succ oldValue, numberOfIncrements = succ oldNumberOfIncrements})

incrementCounterState :: State Counter Int
incrementCounterState = StateT incrementCounter

incrementCounterTwice :: State Counter Int
incrementCounterTwice = incrementCounterState >>= (\_ -> incrementCounterState)

incrementCounterThrice :: State Counter Int
incrementCounterThrice = 
  incrementCounterState >>= 
  \_ -> incrementCounterState >>= 
  \_ -> incrementCounterState

{-
  >:t incrementCounterState
  incrementCounterState :: State Counter Int
  >:t runState incrementCounterState
  runState incrementCounterState :: Counter -> (Int, Counter)


  >let ourCounter=Counter{value=24, numberOfIncrements=3}
  >(runState incrementCounterState) ourCounter
  (24,Counter {value = 25, numberOfIncrements = 4})
-}

{-
  (>>=) :: State s a -> (a -> State s b) -> State s b

  >>= Uses the final state of the first computation as the initial
      state of the second.

  It's implemented like this.

  instance Monad (State s) where
      return a = State $ \s -> (a, s)
      m >>= k = State $ \s -> let (a, s') = runState m s in runState (k a) s'
-}

ret :: a -> State s a
ret x = StateT (\s -> Identity (x, s))

mbind :: State s a -> (a -> State s b) -> State s b
mbind m k = StateT ( \s -> let (a, s') = runState m s in Identity (runState (k a) s') )

incrementCounterTwice' :: State Counter Int
incrementCounterTwice' = mbind incrementCounterState (\_ -> incrementCounterState)

-- *Main Control.Monad.State.Strict> let ourCounter=Counter{value=20, numberOfIncrements=3}
-- *Main Control.Monad.State.Strict> (runState incrementCounterThrice) ourCounter
-- (22,Counter {value = 23, numberOfIncrements = 6})
-- *Main Control.Monad.State.Strict> 
