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

instance Functor (Network d1) where
    fmap g n = (fromFunc g) NN.Network.. n

elementWise :: (A.Applicative a) => (d1 -> d2) -> Network (a d1) (a d2)
elementWise = lift Prelude.. fromFunc

merger :: (d1 -> d2 -> d3) -> Network (d1, d2) d3
merger = fromFunc Prelude.. uncurry

-- utility networks
adder :: (Num d1) => Network (d1, d1) d1
adder = merger (Prelude.+)

multiplier :: (Num d1) => Network (d1, d1) d1
multiplier = merger (Prelude.*)

negater :: (Num d1) => Network d1 d1
negater = fromFunc negate

absoluter :: (Num d1) => Network d1 d1
absoluter = fromFunc abs

signer :: (Num d1) => Network d1 d1
signer = fromFunc signum

fc :: (Num a) => M.Matrix a -> Network (M.Matrix a) (M.Matrix a)
fc w = fromFunc $ (*) w

id :: Network d1 d1
id = fromFunc Prelude.id

split :: Network d1 (d1, d1)
split = fromFunc $ \x -> (x, x)

perm :: (Num a) => Int -> Int -> Network (M.Matrix a) (M.Matrix a)
perm n m = fromFunc $ \w -> (M.permMatrix (M.nrows w) n m) * w

-- operation on network

lift :: (A.Applicative a) => Network d1 d2 -> Network (a d1) (a d2)
lift (Nw f) = fromFunc $ A.liftA f

instance (Num d2) => Num (Network d1 d2) where
    n + m = adder NN.Network.. (n |+| m) NN.Network.. split
    n * m = multiplier NN.Network.. (n|+|m) NN.Network.. split
    abs n = absoluter NN.Network.. n
    signum n = signer NN.Network.. n
    negate n = negater NN.Network.. n
    fromInteger n = fromFunc (\_ -> fromInteger n)

(+) :: (Num d3) => Network d1 d3 -> Network d2 d3 -> Network (d1, d2) d3
n + m = add NN.Network.. (n |+| m)