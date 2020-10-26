{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module Lib where

import qualified Control.Concurrent.Chan       as C
import           GHC.Exts                       ( Constraint )
import           GHC.TypeLits
import           Unsafe.Coerce
import           Prelude                 hiding ( Monad(..) )
import qualified Prelude                       as P
import qualified Control.Concurrent            as Conc
import           Control.Monad.STM
import           Control.Concurrent.STM.TMVar
import           Data.Dynamic
import           System.Exit

class Effect (m :: k -> * -> *) where
   type Unit m :: k
   type Plus m (f :: k) (g :: k) :: k

   return :: a -> m (Unit m) a

   (>>=) :: m f a -> (a -> m g b) -> m (Plus m f g) b

   (>>) :: m f a -> m g b -> m (Plus m f g) b
   x >> y = x >>= (\_ -> y)

newtype EMonad (r :: [*]) a = EMonad { getInner :: IO a }

data NonZero = NZ Int deriving (Show)
{-@ NZ :: {i:Int | i > 0 } -> NonZero @-}

newtype Chan = MkChan (C.Chan Dynamic)

type family Concat (s :: [*]) (t :: [*]) :: r where
            Concat '[] b = b
            Concat  (a ': s) t = Concat s (a ': t)

instance Effect EMonad where
  type Plus EMonad s t = Concat s t
  type Unit EMonad = '[]

  return :: a -> EMonad (Unit EMonad) a
  return a = EMonad (P.return a)

  (>>=) :: EMonad s a -> (a -> EMonad t b) -> EMonad (Plus EMonad s t) b
  x >>= k = EMonad ((P.>>=) (getInner x) (getInner . k))

run :: EMonad '[] a -> IO a
run = getInner

liftIO :: IO a -> EMonad '[] a
liftIO = EMonad

print :: Show a => a -> EMonad '[] ()
print = liftIO . Prelude.print

putStrLn = liftIO . Prelude.putStrLn

send :: Typeable t => Chan -> t -> EMonad '[t] ()
send (MkChan c) t = EMonad $ C.writeChan c (toDyn t)

-- TODO: I'd like to not use exit there, but I'm not sure how to make liquid haskell believe that it is safe
-- (it kinda is not, because it requires writing it somehow that the next value is tied to the monad)
recv :: Typeable t => Chan -> EMonad '[t] t
recv (MkChan c) = EMonad $ do
  v <- C.readChan c
  case fromDynamic v of
    Just t  -> t
    Nothing -> exitWith $ ExitFailure 1
  where (>>=) = (P.>>=)

new :: ((Chan, Chan) -> EMonad env t) -> EMonad env t
new f = EMonad $ C.newChan P.>>= (\c -> getInner $ f (MkChan c, MkChan c))

-- check that both use the same types
type family Same (s :: [*]) (t :: [*]) :: Constraint where
  Same (x ': xs) (y ': ys) = (x ~ y, Same xs ys)
  Same '[] '[] = ()

par :: Same env env' => EMonad env () -> EMonad env' () -> EMonad '[] ()
par (EMonad x) (EMonad y) = EMonad $ do
  res  <- newEmptyTMVarIO
  res' <- newEmptyTMVarIO
  Conc.forkIO (x P.>>= (atomically . putTMVar res))
  Conc.forkIO (y P.>>= (atomically . putTMVar res'))
  () <- atomically $ do
    takeTMVar res
  () <- atomically $ do
    takeTMVar res'
  return ()
 where
  (>>=)  = (P.>>=)
  (>>)   = (P.>>)
  return = P.return

mainFunc = run divProc

divProc =
  new $ \(c, c') -> new $ \(d, d') -> divServer c d `par` divClient c' d'

divServer c d = do
  x      <- recv c
  (NZ y) <- recv c
  send d (x `div` y)

-- divClient :: _ -> _ -> _
divClient c d = do
  send c (2 :: Int)
  send c (NZ 0)
  answer <- recv d
  Lib.putStrLn $ "result " ++ show answer
