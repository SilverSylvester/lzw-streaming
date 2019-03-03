{-
Copyright (c) 2014, jay groven

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of jay groven nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
module Data.ByteString.Arbitrary
( ArbByteString(..)
, ArbByteString1M(..)
, ArbByteString10M(..)
, fastRandBs
, slowRandBs
) where

import Imports
import qualified Relude.Unsafe as Unsafe

import qualified Data.ByteString as BS
import Crypto.Hash.Skein512 ( hash )
import Test.QuickCheck ( Arbitrary(..), Gen, choose, vectorOf )

-- | A ByteString wrapper so we can implement Arbitrary for ByteString. This
-- will currently generate random ByteStrings of length 0 to 100KB.
newtype ArbByteString = ABS { fromABS :: ByteString }
  deriving (Eq, Ord, Read, Show )

-- | A wrapper to generate 1MB bytestrings. The shrink implementation still
-- returns "ArbByteString1M" instances, of course, but they're smaller than
-- 1MB.
newtype ArbByteString1M = ABS1M { fromABS1M :: ByteString }
  deriving (Eq, Ord, Read, Show )

-- | A wrapper to generate 10MB bytestrings. I should really figure out how
-- type-level Nats work, so one can just do (ArbByteStringN 10000000) and have
-- selectable sizes, but I don't see how to do that yet, so 10MB is as big as
-- this library goes. As with the 1MB version, shrink here will generate
-- ArbByteString10M instances that wrap ByteStrings smaller than 10MB.
newtype ArbByteString10M = ABS10M { fromABS10M :: ByteString }
  deriving (Eq, Ord, Read, Show )

instance Arbitrary ArbByteString where
  arbitrary = do
    len <- choose (0, 100*1024)
    ABS `fmap` fastRandBs len

  shrink (ABS bs) = map ABS $ shrinks bs

instance Arbitrary ArbByteString1M where
  arbitrary =
    ABS1M `fmap` fastRandBs (1024*1024)

  shrink (ABS1M bs) = map ABS1M $ shrinks bs

instance Arbitrary ArbByteString10M where
  arbitrary =
    ABS10M `fmap` fastRandBs (10*1024*1024)

  shrink (ABS10M bs) = map ABS10M $ shrinks bs

-- | Generate a bunch of binary data quickly. This abuses the cryptohash skein
-- function to generate a megabyte of data at a time, and then concats chunks
-- until it has enough.
fastRandBs :: Int -> Gen ByteString
fastRandBs len = do
  let perChunk = 1024*1024
  let (rounds, bytes) = len `divMod` perChunk
  bSeed <- slowRandBs $ 16 -- 16 bytes of "really" random seed

  -- Notice the hash (8*) calls; hash always returns an integral number of
  -- bytes (duh), but it wants its output length in bits. We just always track
  -- bytes, and multiply by 8 when calling hash.
  let preChunks = if bytes == 0 then BS.empty else hash (8*bytes) bSeed
  if rounds == 0
    then return preChunks
    else do
      rSeed <- slowRandBs $ 16
      let hashes = Unsafe.tail $ iterate ( hash (8*perChunk) . BS.take 32 ) rSeed
      return $ BS.concat $ preChunks : take rounds hashes

-- | Generate binary data slowly. This generates a list of Word8s, and then
-- uses Data.ByteString.pack to concatenate it into a single ByteString.
slowRandBs :: Int -> Gen ByteString
slowRandBs numBytes = BS.pack `fmap` vectorOf numBytes (choose (0, 255))

shrinks :: ByteString -> [ByteString]
shrinks bs =
  [ BS.append a b | (a, b) <- zip (BS.inits bs) (Unsafe.tail $ BS.tails bs) ]
