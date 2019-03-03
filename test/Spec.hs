import Imports

import Test.QuickCheck
import Data.ByteString.Arbitrary
import Test.QuickCheck.Instances.ByteString

import Data.Functor.Identity
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Streaming as Q

import qualified LZW

-- | NOTE: \NUL causes problems for encode/decode. We therefore assume that any
-- generated string does not have any \NUL character in it, and test whether, in
-- this case, encode and decode invert one another.
prop_encodeDecodeInvertEachOther :: LByteString -> Bool
prop_encodeDecodeInvertEachOther string =
  let result = runIdentity $
          Q.fromLazy string
        & LZW.decode
        & LZW.encode
        & Q.toLazy_
  
  in  B.filter (/= 0) result == B.filter (/= 0) string


-- | Generates reasonably small bytestrings for testing. This catches most of
-- the trivial, small cases.
prop_compressDecompressInvertEachOther :: LByteString -> Bool
prop_compressDecompressInvertEachOther string =
  let compressed = runIdentity $ flip evalStateT LZW.initEncTable
        $ Q.fromLazy string
        & LZW.compress
        & LZW.encode
        & Q.toLazy_

      decompressed = runIdentity $ flip evalStateT LZW.initDecTable
        $ Q.fromLazy compressed
        & LZW.decode
        & LZW.decompress
        & Q.toLazy_

  in  string == decompressed


-- | Generates 1 Mb bytestrings for testing. This takes quite a long time, so we
-- only run 10 tests here. This is mainly to test the capability of the
-- algorithm to run despite running out of table space.
prop_compressDecompressInvertEachOther1M :: ArbByteString1M -> Bool
prop_compressDecompressInvertEachOther1M (ABS1M string) =
  let compressed = runIdentity $ flip evalStateT LZW.initEncTable
        $ Q.fromLazy (fromStrict string)
        & LZW.compress
        & LZW.encode
        & Q.toLazy_

      decompressed = runIdentity $ flip evalStateT LZW.initDecTable
        $ Q.fromLazy compressed
        & LZW.decode
        & LZW.decompress
        & Q.toLazy_

  in  fromStrict string == decompressed


-------------------------------------------------------------------------------

runTests :: IO ()
runTests = do
  quickCheck (withMaxSuccess 1000 prop_encodeDecodeInvertEachOther)
  quickCheck (withMaxSuccess 1000 prop_compressDecompressInvertEachOther)
  quickCheck (withMaxSuccess 10 prop_compressDecompressInvertEachOther1M)

main :: IO ()
main = runTests