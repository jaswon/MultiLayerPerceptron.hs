module Main where

import MLP

import Graphics.Gloss
import Control.Monad.Random (evalRandIO)
import Numeric.LinearAlgebra (matrix, Matrix, fromLists, toLists, (|||), konst)

sdim = 600 :: Float

win :: Display
win = InWindow "XOR Perceptron" (floor sdim,floor sdim) (100,100)

genXORGrid :: Int -> MLP -> [[Double]]
genXORGrid res nn
  = let
    step = recip $ fromIntegral $ res - 1 :: Double
    grid_i = [ [x,y] | x <- [0,step..1] , y <- [0,step..1] ]
  in
    zipWith (++) grid_i ( toLists $ predict nn $ fromLists grid_i ||| konst 1 (res*res,1) )

drawXORInput :: Float -> [Float] -> Picture
drawXORInput dim [x,y,o] 
  = translate (x*sdim) (y*sdim)
  $ color ( greyN o )
  $ rectangleSolid dim dim

drawXOR :: Int -> MLP -> Picture
drawXOR res nn
  = let
    dim = sdim / (fromIntegral $ res-1)
    sfac = (fromIntegral $ res - 1) / (fromIntegral $ res )
  in scale sfac sfac
  $ translate ( -sdim/2 ) ( -sdim/2 )
  $ pictures
  $ fmap ( drawXORInput dim )
  $ fmap (fmap realToFrac)
  $ genXORGrid res nn

main :: IO ()
main = do
  let train_i = matrix 3 [0,0,1,0,1,1,1,0,1,1,1,1]
  let train_o = matrix 1 [0,1,1,0]
  let train_xor = \nn -> fit nn train_i train_o 10
  x <- evalRandIO (initMLP 3 4 1 0.5)
  simulate win white 60 x ( drawXOR 25 ) (\v dt -> train_xor)
