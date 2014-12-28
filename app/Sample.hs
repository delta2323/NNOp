module Main where

import qualified NN.Network as N
import qualified Data.Matrix as M
import qualified Data.Vector as V

n = N.Nw (fmap (+1)) :: N.Network (M.Matrix Float) (M.Matrix Float)
m = N.Nw (fmap (*2)) :: N.Network (M.Matrix Float) (M.Matrix Float)
l = m N.. n
p = m N.|+| n
w = M.matrix 4 3 (\(i, j) -> fromIntegral $ i+j*2)
q = N.fc w
r = m N.|++| n

x1 = M.fromList 3 1 [1, 2, 3]
x2 = M.fromList 3 1 [1, 2, 3]

main = (putStrLn $ show $ N.forward l $ x1)
  >> (putStrLn $ show $ N.forward p $ (x1, x2))
  >> (putStrLn $ show $ N.forward q $ x1)
