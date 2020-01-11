module Idrlisp.Env

import Data.IORef
import Data.SortedMap
import Idrlisp.Monad

%default covering

export
record Env a where
  constructor MkEnv
  parent : Maybe (Env a)
  table : IORef (SortedMap String a)

export
new' : Maybe (Env a) -> IO (Env a)
new' parent = do
  table <- newIORef empty
  pure $ MkEnv parent table

export
new : Maybe (Env a) -> LIO (Env a)
new parent = lift $ new' parent

export
define : String -> a -> Env a -> LIO ()
define key value env =
  lift $ modifyIORef (table env) (insert key value)

export
set : String -> a -> Env a -> LIO ()
set key value env = do
  t <- lift $ readIORef (table env)
  case lookup key t of
    Just _ =>
      lift $ writeIORef (table env) (insert key value t)
    Nothing =>
      case parent env of
        Just e => set key value e
        Nothing => throw $ UndefinedVariable key

export
lookup : String -> Env a -> LIO (Maybe a)
lookup key env = do
  t <- lift $ readIORef (table env)
  case lookup key t of
    Just x => pure $ Just x
    Nothing =>
      case parent env of
        Just e => lookup key e
        Nothing => pure Nothing

export
get : String -> Env a -> LIO a
get key env = do
  x <- lookup key env
  case x of
    Just x => pure x
    Nothing => throw $ UndefinedVariable key

