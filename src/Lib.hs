module Lib
    ( someFunction
      , StateT(..)
      , AppState(..)
    ) where

----------------------------------
newtype State s a = State { runState :: s -> (a, s) }
----------------------------------

instance Functor (State s) where
  fmap f (State g) = State $ \i ->
    let (r, i') = g i
    in (f r, i')

instance Applicative (State s) where
  pure x = State $ \i -> (x, i)
  (State g) <*> (State h) = State $ \i ->
    let (f, i') = g i
        (x, i'') = h i'
    in (f x, i'')

instance Monad (State s) where
  (State g) >>= f = State $ \i ->
    let (a, i') = g i
        h = runState $ f a
        (a', i'') = h i'
    in (a', i'')

----------------------------------

get :: State s s
get = State $ \i -> (i, i)

put :: s -> State s ()
put x = State $ \_ -> ((), x)

----------------------------------
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
----------------------------------

instance Monad m => Functor (StateT s m) where
  fmap f (StateT g) = StateT $ \i -> do
    (r, i') <- g i
    return $ (f r, i')

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \i -> return (x, i)
  (StateT g) <*> (StateT h) = StateT $ \i -> do
    (f, i') <- g i
    (x, i'') <- h i'
    return $ (f x, i'')

instance Monad m => Monad (StateT s m) where
  (StateT g) >>= f = StateT $ \i -> do
    (a, i') <- g i
    let h = runStateT $ f a
    (a', i'') <- h i'
    return $ (a', i'')

----------------------------------

tGet :: Monad m => StateT s m s
tGet = StateT $ \i -> return (i, i)

tGets :: Monad m => (s -> b) -> StateT s m b
tGets f = StateT $ \i -> return (f i, i)

tPut :: Monad m => s -> StateT s m ()
tPut x = StateT $ \_ -> return ((), x)

----------------------------------
----------------------------------

data AppState = AppState { getX :: Int, getY :: Int, getZ :: Int } deriving Show

someFunction :: StateT AppState (Either String) Int
someFunction = do
  x <- tGets getX
  y <- tGets getY
  if y == 0 then errorResult "y is zero"
            else do
              let z = x `div` y
              tPut $ AppState x y z
              return z

errorResult :: String -> StateT AppState (Either String) Int
errorResult msg = StateT $ \_ -> Left msg
