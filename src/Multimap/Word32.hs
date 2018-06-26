module Multimap.Word32
  ( new
  , insert
  , lookup
  , Multimap
  ) where

import           Data.Word

import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

import           Prelude hiding (length, lookup)

data    CMultimap
newtype Multimap = Multimap (ForeignPtr CMultimap)

foreign import ccall unsafe "new_mmap_int"     new_mmap :: IO (Ptr CMultimap)
foreign import ccall unsafe "&delete_mmap_int" delete_mmap
  :: FunPtr (Ptr CMultimap -> IO ())
foreign import ccall unsafe "insert_mmap_int"  insert_mmap
  :: Ptr CMultimap
  -> Word32
  -> Word32
  -> IO ()
foreign import ccall unsafe "lookup_mmap_int"  lookup_mmap
  :: Ptr CMultimap
  -> Word32
  -> IO (Ptr Word32)
foreign import ccall unsafe "free_result"      free_result
  :: Ptr Word32
  -> IO ()

new :: IO Multimap
new = do
  mm <- new_mmap
  Multimap <$> newForeignPtr delete_mmap mm

insert :: Multimap -> Word32 -> Word32 -> IO ()
insert (Multimap mm) k v = withForeignPtr mm $ \mm' -> insert_mmap mm' k v

lookup :: Multimap -> Word32 -> IO [Word32]
lookup (Multimap mm) k = withForeignPtr mm $ \mm' -> do
  result <- lookup_mmap mm' k
  length <- peek result
  array  <- peekArray (fromIntegral length) result

  free_result result
  pure $ drop 1 array
