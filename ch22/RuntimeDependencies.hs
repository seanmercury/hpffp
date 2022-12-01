{-# LANGUAGE ScopedTypeVariables #-}
import Database.PostgreSQL.Simple.Internal (Connection)
import Network.Socket
import Data.Text 
import Prelude 
import Control.Monad

data AppCtx = AppCtx
  { appConn :: Connection
  , appSock :: Socket
  , appPutLog :: Text -> IO ()
  , appAwaitLog :: IO ()
  }

newtype App a = App (AppCtx -> IO a)

instance Functor App where
  fmap :: (a -> b) -> App a -> App b
  fmap f (App fa) = App $ fmap f . fa

instance Applicative App where
  pure :: a -> App a
  pure fa = App $ \_ -> pure fa

  (<*>) :: App (a -> b) -> App a -> App b  -- a :: AppCtx -> IO a, b :: AppCtx -> IO b
  (<*>) (App f) (App a) = App $ \c -> f c <*> a c   -- f c :: IO (a -> b), a c :: IO a
  -- (<*>) (App f) (App a) = App $ \c -> let 
  --   fc = f c
  --   ac = a c
  --   in fc <*> ac

instance Monad App where
  (>>=) :: forall a b. App a -> (a -> App b) -> App b   -- a :: AppCtx -> IO a, b :: AppCtx -> IO b, 
                                            -- g :: AppCtx -> IO a, f :: a -> App b 
  
  (>>=) (App g) f = App $ \c -> (\(App k) -> k c) =<< f <$> g c

  -- (>>=) (App g) f = App $ \c -> let
  --   gc :: IO a
  --   gc = g c -- gc :: IO a

  --   fgc :: IO (App b)
  --   fgc = f <$> gc  -- fgc :: IO (App b)

  --   appBToIoB :: App b -> IO b
  --   appBToIoB (App f) = f c

  --   in appBToIoB =<< fgc

  -- (>>=) (App g) f = App $ \c -> do
  --   gc <- g c 
  --   let fgc = f gc  -- f :: a -> App b, gc a
  --   let appBToIoB (App f) = f c
  --   appBToIoB fgc