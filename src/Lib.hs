{-# LANGUAGE DataKinds, TypeFamilies, UndecidableInstances, PolyKinds, PartialTypeSignatures, RankNTypes #-}

module Lib where

import           Prelude

newtype EMonad (r :: ()) a = EMonad { getInner :: IO a }

type family Plus (e :: k -> * -> *) (f :: k) (g :: k) :: k where
  Plus EMonad _ _ = '()

type family Unit (e :: k -> * -> *) :: k where
  Unit EMonad = '()

data NonZero = NZ Int deriving (Show)
{-@ NZ :: {i:Int | i > 0 } -> NonZero @-}

-- with this annotation is safe (without the divClient annotations)
eBind :: EMonad s a -> (a -> EMonad t b) -> EMonad (Plus EMonad s t) b
-- with this one is unsafe regardless of divClient's
-- eBind :: EMonad s a -> (a -> EMonad t b) -> EMonad '() b
-- without any annotations here is also unsafe regardless of divClient's
eBind x k = EMonad ((>>=) (getInner x) (getInner . k))

-- with any of this annotations is unsafe (without it is safe)
-- divClient ::  Int -> EMonad '() ()
-- divClient ::  _ -> _
divClient _ = send (NZ 0) `eBind` (\_ -> ret ())

send :: t -> EMonad '() ()
send _ = EMonad $ return ()

ret :: a -> EMonad (Unit EMonad) a
ret a = EMonad (return a)