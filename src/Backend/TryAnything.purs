module TryAnything where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import FRP.Event (create, subscribe)

trial :: Effect Unit
trial = do
  { event: e1, push: push1 } <- create
  { event: e2, push: push2 } <- create
  _ <- subscribe e1 \s -> s "hebele"
  _ <- subscribe e2 \i -> i 42
  let e3 = Tuple <$> e1 <*> e2
  _ <- subscribe e3 \(Tuple s i) -> s "höbölö" *> i 4242

  push1 $ \v -> Console.log $ "e1 a " <> v
  push1 $ \v -> Console.log $ "e1 b " <> v
  push2 \v -> Console.log $ "e2 1 " <> show v
  push2 \v -> Console.log $ "e2 2 " <> show v
  push1 \v -> Console.log $ "e1 c " <> v
  push2 \v -> Console.log $ "e2 3 " <> show v