module Main where

import Control.Monad

import qualified Data.Vector.Storable as S
import Data.Word

import Pipes
import qualified Pipes.Prelude as Pipes

import Camera

main :: IO ()
main = do
  mCamera <- cameraProducer
  case mCamera of
    Nothing -> putStrLn "Camera not found"
    Just (serial,dim,producer) ->
      do
        putStrLn $ "Camera Serial: " ++ show serial
        putStrLn $ "Running at dimension: " ++ show dim
        runEffect $ producer >-> Pipes.take 20 >-> consumer


-- |consumer just consumes frames
-- and prints the sum of all the pixels mod 255 (Word8 overflow)
consumer :: Consumer (S.Vector Word8) IO ()
consumer =
  forever $
  do
    v <- await
    Pipes.lift $ print $ S.sum v
