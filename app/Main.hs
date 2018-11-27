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
        runEffect $ producer >-> Pipes.take 20 >-> consumer

consumer :: Consumer (S.Vector Word8) IO ()
consumer =
  forever $
  do
    v <- await
    liftIO $ print $ S.length v
    Pipes.lift $ print $ S.sum v
