module Multimap.ByteString
  ( new
  , insert
  , lookup
  , Multimap
  ) where

import qualified Data.ByteString as B
import           Data.Word

import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.C.String
import           Foreign.Storable

import           Prelude hiding (lookup)

data    CMultimap
newtype Multimap = Multimap (ForeignPtr CMultimap)

foreign import ccall unsafe "new_mmap_str"     new_mmap :: IO (Ptr CMultimap)
foreign import ccall unsafe "&delete_mmap_str" delete_mmap
  :: FunPtr (Ptr CMultimap -> IO ())
foreign import ccall unsafe "insert_mmap_str"  insert_mmap
  :: Ptr CMultimap
  -> CString
  -> Word32
  -> IO ()
foreign import ccall unsafe "lookup_mmap_str"  lookup_mmap
  :: Ptr CMultimap
  -> CString
  -> IO (Ptr Word32)
foreign import ccall unsafe "free_result"      free_result
  :: Ptr Word32
  -> IO ()

new :: IO Multimap
new = do
  mm <- new_mmap
  Multimap <$> newForeignPtr delete_mmap mm

insert :: Multimap -> B.ByteString -> Word32 -> IO ()
insert (Multimap mm) k v = withForeignPtr mm $ \mm' ->
  B.useAsCString k $ \k' -> insert_mmap mm' k' v

lookup :: Multimap -> B.ByteString -> IO [Word32]
lookup (Multimap mm) k = withForeignPtr mm $ \mm' -> do
  result <- B.useAsCString k $ \k' -> lookup_mmap mm' k'
  length <- peek result
  array  <- peekArray (fromIntegral length) result

  free_result result
  pure $ drop 1 array
