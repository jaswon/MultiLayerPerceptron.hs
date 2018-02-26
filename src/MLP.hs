module MLP ( MLP
           , initMLP
           , fit
           , predict
           ) where

import System.Random (RandomGen)
import Control.Monad.Random.Lazy (Rand, getRandoms, evalRandIO)
import Numeric.LinearAlgebra (Matrix, (><), (<>), cmap, matrix, tr)

-- Activation function

activate :: Double -> Double
activate a = 1/(1+exp (-a))

-- derivative of activation function w.r.t. activation function
activate' :: Double -> Double
activate' a = a * (1 - a)

-- Multi Layer Perceptron
data MLP = MLP
  { ih :: Matrix Double
  , ho :: Matrix Double
  , lr :: !Double
  } deriving (Eq, Show)

initMLP :: RandomGen g => Int -> Int -> Int -> Double -> Rand g MLP
initMLP i h o lr = fmap (mkMLP i h o lr) getRandoms

mkMLP :: Int -> Int -> Int -> Double -> [Double] -> MLP
mkMLP i h o lr initWeights
  = let
    ( ihw,nextWeights ) = splitAt (i*h) initWeights
    how = take (h*o) nextWeights
    w = (i><h) $ map (\x -> 2 * x - 1) ihw 
    v = (h><o) $ map (\x -> 2 * x - 1) how
  in (MLP w v lr)

fit :: MLP -> Matrix Double -> Matrix Double -> Int -> MLP
fit nn _ _ 0 = nn
fit nn x y epochs
  = let
    h_out = cmap activate $ x <> (ih nn)
    out = cmap activate $ h_out <> (ho nn)
    ho_delta = (y - out) * (cmap activate' out)
    ih_delta = ( ho_delta <> (tr $ ho nn)) * (cmap activate' h_out) 
    new_ho = (+) (ho nn) $ cmap (* (lr nn)) $ (tr h_out) <> ho_delta
    new_ih = (+) (ih nn) $ cmap (* (lr nn)) $ (tr x) <> ih_delta
  in fit (MLP new_ih new_ho (lr nn)) x y (epochs-1)

predict :: MLP -> Matrix Double -> Matrix Double
predict nn x
  = let
    h_out = cmap activate $ x <> (ih nn)
    out = cmap activate $ h_out <> (ho nn)
  in out
