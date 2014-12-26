module NN.Network where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Control.Applicative as A

type Blob = [Float]
data Network d1 d2 = Nw (d1 -> d2)

class INetwork n where
    fromFunc :: (d1 -> d2) -> n d1 d2
    forward :: n d1 d2 -> d1 -> d2
    (.) :: n d2 d3 -> n d1 d2 -> n d1 d3
    (|+|) :: n d1 d3 -> n d2 d4 -> n (d1, d2) (d3, d4)

instance INetwork Network where
    fromFunc f = Nw f
    forward (Nw f) = f
    (Nw g) . (Nw f) = Nw (g Prelude.. f)
    (Nw f) |+| (Nw g) = Nw (\(x, y) -> (f x, g y))

instance Functor (Network n) where
    fmap g (Nw f) = Nw $ g Prelude.. f

fc :: (Num a) => M.Matrix a -> Network (M.Matrix a) (M.Matrix a)
fc w = Nw $ (*) w

lift :: (A.Applicative a) => Network d1 d2 -> Network (a d1) (a d2)
lift (Nw f) = Nw $ A.liftA f

elementWise :: (A.Applicative a) => (d1 -> d2) -> Network (a d1) (a d2)
elementWise f = lift $ Nw f

elementWiseAdd :: (Num d1) => Network (d1, d1) d1
elementWiseAdd = Nw $ \(x, y) -> (x Prelude.+ y)

(+) :: (Num d3) => Network d1 d3 -> Network d2 d3 -> Network (d1, d2) d3
(Nw f) + (Nw g) = elementWiseAdd NN.Network.. ((Nw f) |+| (Nw g))