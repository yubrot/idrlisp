module Idrlisp.Env

import Data.IORef
import Data.SortedMap

%default covering

export
record Env a where
  constructor MkEnv
  parent : Maybe (Env a)
  table : IORef (SortedMap String a)

export
new : Maybe (Env a) -> IO (Env a)
new parent = do
  table <- newIORef empty
  pure $ MkEnv parent table

export
define : String -> a -> Env a -> IO ()
define key value env = modifyIORef (table env) (insert key value)

export
set : String -> a -> Env a -> IO (Either String ())
set key value env = do
  t <- readIORef (table env)
  case lookup key t of
    Just _ => do
      writeIORef (table env) (insert key value t)
      pure $ Right ()
    Nothing =>
      case parent env of
        Just e => set key value e
        Nothing => pure $ Left key

export
lookup : String -> Env a -> IO (Either String a)
lookup key env = do
  t <- readIORef (table env)
  case lookup key t of
    Just x => pure $ Right x
    Nothing =>
      case parent env of
        Just e => lookup key e
        Nothing => pure $ Left key

