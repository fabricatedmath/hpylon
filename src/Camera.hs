module Camera (cameraProducer) where

import Control.Applicative
import Control.DeepSeq (deepseq)

import Control.Monad
import Data.Maybe

import Data.Vector.Storable as S

import Data.Word

import Foreign.ForeignPtr
import Foreign.Marshal.Utils

import Pipes

import Prelude as P

import Camera.FFI

type DIM2 = (Int,Int)

type SerialNumber = Word64

cameraProducer :: IO (Maybe (SerialNumber, DIM2, Producer (S.Vector Word8) IO ()))
cameraProducer =
  do
    mcfptr <- camera
    case mcfptr of
      Nothing -> return Nothing
      Just cfptr ->
        do
          ser <- withForeignPtr cfptr serial
          dim <- cameraDim cfptr
          let
            s = arraySize dim
            produce =
              do
                marr <-
                  liftIO $
                  withForeignPtr cfptr
                  (\cptr ->
                     do
                       mptr <- grab cptr
                       maybePeek
                         (\ptr ->
                          do
                              fptr <- newForeignPtr_ ptr
                              return $ S.unsafeFromForeignPtr0 fptr s
                         ) mptr)
                maybe (return ()) (\x -> x `deepseq` yield x) marr
                produce
          return $ Just (ser,dim,produce)

cameraDim :: ForeignPtr Camera -> IO DIM2
cameraDim cfptr =
  do
    h <- P.fromIntegral <$> withForeignPtr cfptr height
    w <- P.fromIntegral <$> withForeignPtr cfptr width
    return (h,w)

arraySize :: DIM2 -> Int
arraySize (h,w) = h*w
