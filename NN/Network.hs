module NN.Network where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Control.Applicative as A

type Blob = [Float]
data Network d1 d2 = Nw (d1 -> d2)

class INetwork n where
    forward :: n d1 d2 -> d1 -> d2
    (.) :: n d2 d3 -> n d1 d2 -> n d1 d3
    (|+|) :: (Num d3) => n d1 d3 -> n d2 d3 -> n (d1, d2) d3

instance INetwork Network where
    forward (Nw f) = f
    (Nw g) . (Nw f) = Nw (g Prelude.. f)
    (Nw f) |+| (Nw g) = Nw (\(x, y) -> f x + g y)

instance (Num y) => Num (x -> y) where
    f + g = \x -> f x + g x
    f * g = \x -> f x * g x
    abs f = abs Prelude.. f
    fromInteger n = \_ -> fromInteger n
    signum f = signum Prelude.. f
    negate f = negate Prelude.. f

instance (Num d2) => Num (Network d1 d2) where
    (Nw f) + (Nw g) = Nw (f + g)
    (Nw f) * (Nw g) = Nw (f * g)
    abs (Nw f) = Nw (abs f)
    fromInteger n = Nw (fromInteger n)
    signum (Nw f) = Nw (signum f)
    negate (Nw f) = Nw (negate f)

-- instance (A.Applicative a, Num x) => Num a x where
--      as + bs = A.liftA2 (+) as bs
--      as * bs = A.liftA2 (*) as bs
--      abs as = A.liftA abs as
--      fromInteger n = A.pure $ fromInteger n
--      signum as = A.liftA signum as
--      negate as = A.liftA negate as

fc :: (Num a) => M.Matrix a -> Network (M.Matrix a) (M.Matrix a)
fc w = Nw((*) w)

lift :: (A.Applicative a) => Network d1 d2 -> Network (a d1) (a d2)
lift (Nw f) = Nw (A.liftA f)

