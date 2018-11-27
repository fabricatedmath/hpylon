module Camera.FFI where

import Data.Word (Word8,Word32,Word64)

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Utils (maybePeek)

foreign import ccall unsafe "Camera_setStrategyOneByOne" c_setStrategyOneByOne
  :: Ptr Camera -> IO ()
foreign import ccall unsafe "Camera_setStrategyLatestImageOnly" c_setStrategyLatestImageOnly
  :: Ptr Camera -> IO ()
foreign import ccall unsafe "Camera_new" c_new
  :: IO (Ptr Camera)
foreign import ccall unsafe "Camera_height" c_getHeight
  :: Ptr Camera -> IO Word32
foreign import ccall unsafe "Camera_width" c_getWidth
  :: Ptr Camera -> IO Word32
foreign import ccall unsafe "Camera_serial" c_getSerial
  :: Ptr Camera -> IO Word64
foreign import ccall unsafe "Camera_grab" c_grab
  :: Ptr Camera -> IO (Ptr Word8)
foreign import ccall unsafe "&Camera_delete" c_delete
  :: FinalizerPtr Camera

data Camera

camera :: IO (Maybe (ForeignPtr Camera))
camera =
  do
    cameraPtr <- c_new
    maybePeek (newForeignPtr c_delete) cameraPtr

setStrategyOneByOne :: Ptr Camera -> IO ()
setStrategyOneByOne = c_setStrategyOneByOne

setStrategyLatestImageOnly :: Ptr Camera -> IO ()
setStrategyLatestImageOnly = c_setStrategyLatestImageOnly

height :: Ptr Camera -> IO Word32
height = c_getHeight

width :: Ptr Camera -> IO Word32
width = c_getWidth

serial :: Ptr Camera -> IO Word64
serial cameraPtr = c_getSerial cameraPtr

grab :: Ptr Camera -> IO (Ptr Word8)
grab = c_grab
