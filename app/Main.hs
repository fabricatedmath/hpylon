module Main where

import Control.Monad

import qualified Data.Vector.Storable as S
import Data.Word

import Pipes

import Camera

main :: IO ()
main = do
  mCamera <- cameraProducer
  case mCamera of
    Nothing -> putStrLn "Camera not found"
    Just (dim,producer) ->
      do
        print dim
        runEffect $ producer >-> consumer

consumer :: Consumer (S.Vector Word8) IO ()
consumer =
  forever $
  do
    v <- await
    liftIO $ print $ S.length v
    Pipes.lift $ print $ S.sum v
