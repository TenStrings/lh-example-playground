{-# LANGUAGE DataKinds, InstanceSigs, RebindableSyntax, TypeFamilies, UndecidableInstances, PolyKinds, PartialTypeSignatures #-}

module Lib where

import           GHC.Exts                       ( Constraint )
import           Prelude                 hiding ( Monad(..) )
import qualified Prelude                       as P

class Effect (m :: k -> * -> *) where
   type Unit m :: k
   type Plus m (f :: k) (g :: k) :: k

   return :: a -> m (Unit m) a

   (>>=) :: m f a -> (a -> m g b) -> m (Plus m f g) b

   (>>) :: m f a -> m g b -> m (Plus m f g) b
   x >> y = x >>= (\_ -> y)

newtype EMonad (r :: ()) a = EMonad { getInner :: IO a }

instance Effect EMonad where
  type Plus EMonad s t = '()
  type Unit EMonad = '()

  return :: a -> EMonad (Unit EMonad) a
  return a = EMonad (P.return a)

  (>>=) :: EMonad s a -> (a -> EMonad t b) -> EMonad (Plus EMonad s t) b
  x >>= k = EMonad ((P.>>=) (getInner x) (getInner . k))

send ::  t -> EMonad '() ()
send _ = EMonad $ P.return ()

data NonZero = NZ Int deriving (Show)
{-@ NZ :: {i:Int | i > 0 } -> NonZero @-}

mainFunc = getInner $ divClient 1

-- uncommenting any of these makes the code unsafe, otherwise it is safe
-- divClient :: _ -> _
-- divClient ::  Int -> EMonad '() ()
-- {-@ divClient ::  forall p. p -> EMonad _ () @-}

divClient _ = do
  send (NZ 0)
  return ()