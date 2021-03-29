
{-# LANGUAGE DataKinds, RankNTypes, InstanceSigs, PartialTypeSignatures, RebindableSyntax, TypeFamilies, TypeOperators, UndecidableInstances, PolyKinds, GADTs #-}

module Ex2 where

import qualified Control.Concurrent.Chan       as C
import           GHC.Exts                       ( Constraint )
import           GHC.TypeLits                   ( Nat )
import           Unsafe.Coerce                  ( unsafeCoerce )
import           Prelude                 hiding ( Monad(..), putStrLn )
import qualified Prelude                       as P
import qualified Control.Concurrent            as Conc
import           Control.Monad.STM              ( atomically )
import           Control.Concurrent.STM.TMVar   ( newEmptyTMVarIO
                                                , putTMVar
                                                , takeTMVar
                                                )
import Data.Set

class Effect (m :: k -> * -> *) where
   type Unit m :: k
   type Plus m (f :: k) (g :: k) :: k

   return :: a -> m (Unit m) a

   (>>=) :: m f a -> (a -> m g b) -> m (Plus m f g) b

   (>>) :: m f a -> m g b -> m (Plus m f g) b
   x >> y = x >>= (\_ -> y)

newtype EMonad (r :: [Op *]) a = EMonad { getInner :: IO a }

data Chan = forall a. MkChan { chan:: C.Chan a, ghost:: Set (Int, Int) }

data NonZero = NZ Int deriving (Show)
{-@ NZ :: {i:Int | i > 0 } -> NonZero @-}

data Op a = Send a | Receive a

type family Concat (s :: [Op *]) (t :: [Op *]) :: r where
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
print = liftIO . P.print

putStrLn = liftIO . P.putStrLn

send :: Chan -> t -> EMonad '[ 'Send t] ()
send (MkChan c _) t = EMonad $ C.writeChan (unsafeCoerce c) t

recv :: Chan -> EMonad '[ 'Receive t] t
recv (MkChan c _) = EMonad $ do
  C.readChan (unsafeCoerce c)

new :: ((Chan, Chan) -> EMonad env t) -> EMonad env t
new f = EMonad $ C.newChan P.>>= (\c -> getInner $ f (MkChan c empty, MkChan c empty))

type family Dual s where
  Dual ('Send a) = 'Receive a
  Dual ('Receive a) = 'Send a

type family Balanced (s :: [Op *]) (t :: [Op *]) :: Constraint where
  Balanced (x ': xs) (y ': ys) = (x ~ Dual y, Balanced xs ys)
  Balanced '[] '[] = ()

par :: Balanced env env' => EMonad env () -> EMonad env' () -> EMonad '[] ()
par (EMonad x) (EMonad y) = EMonad $ do
  res  <- newEmptyTMVarIO
  res' <- newEmptyTMVarIO
  _ <- Conc.forkIO (x P.>>= (atomically . putTMVar res))
  _ <- Conc.forkIO (y P.>>= (atomically . putTMVar res'))
  () <- atomically $ do
    takeTMVar res
  () <- atomically $ do
    takeTMVar res'
  return ()
 where
  (>>=)  = (P.>>=)
  --(>>)   = (P.>>)
  return = P.return

mainFunc :: IO ()
mainFunc = run divProc

divProc :: EMonad '[] ()
divProc =
  new $ \(c, c') -> new $ \(d, d') -> divServer c d `par` divClient c' d'

-- divServer :: Chan -> Chan -> EMonad '[ 'Receive Int, 'Receive NonZero, 'Send (CheckedDivT 4 2)] ()
-- unsound u.u
divServer :: Chan -> Chan -> EMonad '[ 'Receive Int, 'Receive NonZero, 'Send Int] ()
divServer c d = do
  x      <- recv c
  (NZ y) <- recv c
  send d (x `div` y)

divClient :: Chan -> Chan -> EMonad '[ 'Send Int, 'Send NonZero, 'Receive Int] ()
divClient c d = do
  send c (4 :: Int)
  send c (NZ 2)
  answer <- recv d
  putStrLn $ "answer " ++ show answer
