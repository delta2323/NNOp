module Main where

import qualified NN.Network as N
import qualified Data.Matrix as M
import qualified Data.Vector as V

n = N.Nw (fmap (+1)) :: N.Network (M.Matrix Float) (M.Matrix Float)
m = N.Nw (fmap (*2)) :: N.Network (M.Matrix Float) (M.Matrix Float)
l = m N.. n
p = m N.|+| n
w = M.matrix 4 3  (\(i, j) -> i+j*2)
q = N.fc w
r = m N.+ n

-- main = putStrLn $ show $ N.forward p [2, 3]
main = putStrLn $ show $ N.forward q $ M.fromList 3 1 [1, 2, 3]