{-# LANGUAGE DataKinds, InstanceSigs, PartialTypeSignatures, RebindableSyntax, TypeFamilies, TypeOperators, UndecidableInstances, PolyKinds, GADTs #-}

module Ex1 where

import qualified Control.Concurrent.Chan       as C
import           GHC.Exts                       ( Constraint )
import           GHC.TypeLits                   ( Nat )
import           Unsafe.Coerce                  ( unsafeCoerce )
import           Prelude                 hiding ( Monad(..) )
import qualified Prelude                       as P
import qualified Control.Concurrent            as Conc
import           Control.Monad.STM              ( atomically )
import           Control.Concurrent.STM.TMVar   ( newEmptyTMVarIO
                                                , putTMVar
                                                , takeTMVar
                                                )
import           Data.Dynamic                   ( toDyn
                                                , Typeable
                                                , fromDynamic
                                                , Dynamic
                                                )
import           System.Exit                    ( exitWith
                                                , ExitCode(ExitFailure)
                                                )

class Effect (m :: k -> * -> *) where
   type Unit m :: k
   type Plus m (f :: k) (g :: k) :: k

   return :: a -> m (Unit m) a

   (>>=) :: m f a -> (a -> m g b) -> m (Plus m f g) b

   (>>) :: m f a -> m g b -> m (Plus m f g) b
   x >> y = x >>= (\_ -> y)

newtype EMonad (r :: [Op *]) a = EMonad { getInner :: IO a }

data Chan = forall a. MkChan (C.Chan a)

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
print = liftIO . Prelude.print

putStrLn = liftIO . Prelude.putStrLn

send :: Chan -> t -> EMonad '[Send t] ()
send (MkChan c) t = EMonad $ C.writeChan (unsafeCoerce c) t

recv :: Chan -> EMonad '[Receive t] t
recv (MkChan c) = EMonad $ do
  C.readChan (unsafeCoerce c)

new :: ((Chan, Chan) -> EMonad env t) -> EMonad env t
new f = EMonad $ C.newChan P.>>= (\c -> getInner $ f (MkChan c, MkChan c))

type family Dual s where
  Dual (Send a) = Receive a
  Dual (Receive a) = Send a

type family Balanced (s :: [Op *]) (t :: [Op *]) :: Constraint where
  Balanced (x ': xs) (y ': ys) = (x ~ Dual y, Balanced xs ys)
  Balanced '[] '[] = ()

par :: Balanced env env' => EMonad env () -> EMonad env' () -> EMonad '[] ()
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

-- uncommenting any of these makes the code unsafe, otherwise it is safe
divClient :: _ -> _ -> _
-- divClient
--   :: Show a
--   => Chan
--   -> Chan
--   -> EMonad '[ 'Send Int, 'Send NonZero, 'Receive a] ()
divClient c d = do
  send c (2 :: Int)
  send c (NZ 0)
  answer <- recv d
  Lib.putStrLn $ "answer " ++ show answer