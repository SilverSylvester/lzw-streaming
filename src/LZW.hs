{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : LZW
Description : Streaming LZW compression
Copyright   : (c) Conor Reynolds, 2019
License     : MIT
Maintainer  : reynolds.conor@gmail.com
Stability   : stable
Portability : unknown

Exposes four main functions: 'encode', 'decode', 'compress', and 'decompress'. 
-}
module LZW where


import           Imports
import qualified Relude.Unsafe                 as Unsafe

import           Data.Bits
import qualified Data.Binary                   as Binary
import           Data.Word
import qualified Data.ByteString               as B

import           Data.Trie                     ( Trie )
import qualified Data.Trie                     as T
import           Data.IntMap                   (IntMap)
import qualified Data.IntMap                   as M

import           Streaming
import qualified Streaming.Prelude             as S
import qualified Data.ByteString.Streaming     as Q

import qualified System.IO                     as IO


infixl 9 !

-- | Unsafe trie lookup. Convenient when you are certain the lookup cannot fail,
-- although usually this is discouraged.
(!) :: Trie a -> ByteString -> a
(!) t bs = Unsafe.fromJust $ T.lookup bs t


-- | Decodes a stream of integer codes as 16 bit words.
decode :: Monad m => Stream (Of Int) m r -> Q.ByteString m r
decode = Q.fromChunks . S.map toBinary
  where
    toBinary code = 
      let bytes = map fromIntegral
            [  code .&. 0xFF
            , (code .&. 0xFF00) `shiftR` 8
            ]
      in  B.pack bytes


-- | Encodes a bytestream (chunked by 16 bits) into a stream of codes for
-- decompression.
encode :: Monad m => Q.ByteString m r -> Stream (Of Int) m r
encode = S.unfoldr loop . mapped S.toList . chunksOf 2 . Q.unpack
  where                                 --  ^^^^^^^^^^ guarantees that the list has 1 or 2 elements.
    loop s = do
      S.next s >>= \case
        Right (bytes, rest) -> do
          let code =
                case bytes of
                  [w1, w2]     -> fromIntegral w1 + fromIntegral w2 `shiftL` 8
                  [w1]         -> fromIntegral w1
                  _            -> error "Unreachable"
          return $ Right (code, rest)

        Left r ->
          return $ Left r


-- | Encoding Table data structure; a mapping between byte sequences and codes.
-- A regular 'HashMap' doesn't carry size information, so we have to do that
-- manually.
data EncTable = EncTable
  { encTable :: Trie Int
  , encSize  :: {-# UNPACK #-} !Int
  }


-- | Initial 'EncTable' filled with all single-character codes.
initEncTable :: EncTable
initEncTable = EncTable
  { encTable = T.fromList [(B.singleton n, fromIntegral n) | n <- [0..]]
  , encSize  = fromIntegral (maxBound :: Word8)
  }


-- | Updates the encoding table by adding the new bytestring as key to the value
-- "size + 1".
updateEncTable :: ByteString -> EncTable -> EncTable
updateEncTable s table = table
  { encTable = T.insert s (encSize table + 1) (encTable table)
  , encSize  = encSize table + 1
  }


-- | Compression stream transformer. "Compresses" a byte stream into a stream
-- consisting of the output codes.
compress :: MonadState EncTable m => Q.ByteString m () -> Stream (Of Int) m ()
compress = S.unfoldr (loop []) . Q.unpack
  where
    loop bytes s = do
      -- Obtain current table from state
      table <- gets encTable
      size  <- gets encSize
      S.next s >>= \case
        -- 'byte' == next byte in the stream
        -- 'rest' == the rest of the stream
        Right (byte, rest) -> do
          let newBytes = bytes ++ [byte]
              oldBS    = B.pack bytes
              newBS    = B.pack newBytes

          if T.member newBS table
            then loop newBytes rest
            else do
              let code = table ! oldBS
              when (size < 65535) $
                modify (updateEncTable newBS)
              return $ Right (code, S.cons byte rest)
        
        -- no bytes left to process, so consume whatever bytes are still held
        -- locally
        Left () ->
          if null bytes
            then return $ Left ()
            else return $ Right (table ! B.pack bytes, s)

-- | Decoding table. This table needs to also keep track of a previously seen
-- bytestring for later processing.
data DecTable = DecTable
  { decTable    :: IntMap ByteString
  , decPrevious :: !ByteString
  , decSize     :: {-# UNPACK #-} !Int
  }


-- | Initial decoding table.
initDecTable :: DecTable
initDecTable = DecTable
  { decTable    = M.fromList [(fromIntegral n, B.singleton n) | n <- [0..]]
  , decPrevious = B.empty
  , decSize     = fromIntegral (maxBound :: Word8)
  }


-- | Update a decoding table with both a new string and a new previous.
updateDecTable :: ByteString -> ByteString -> DecTable -> DecTable
updateDecTable s previous dt = dt
  { decTable    = M.insert (decSize dt + 1) s (decTable dt)
  , decPrevious = previous
  , decSize     = decSize dt + 1
  }


-- | Update a decoding table with just a new previous.
updatePrevious :: ByteString -> DecTable -> DecTable
updatePrevious previous dt = dt { decPrevious = previous }


-- | Decompression stream transformer. "Decompresses" a stream of ints into a
-- byte stream.
decompress :: MonadState DecTable m => Stream (Of Int) m () -> Q.ByteString m ()
decompress = Q.fromChunks . S.unfoldr loop
  where
    loop s = do
      table    <- gets decTable
      previous <- gets decPrevious
      size     <- gets decSize

      S.next s >>= \case
        Right (code, rest) -> do
          if B.null previous
            then do
              when (size < 65535) $
                modify $ updatePrevious (table M.! code)
              return $ Right (table M.! code, rest)

            else do
              let entry = fromMaybe (previous `B.snoc` B.head previous) (M.lookup code table)
              if size < 65535
                then modify $ updateDecTable (previous `B.snoc` B.head entry) entry
                else modify $ updatePrevious entry
              return $ Right (entry, rest)

        Left () ->
          return $ Left ()


-- | Compresses data on arrival from stdin. For demonstration purposes. Note
-- that setting 'IO.NoBuffering' ensures that each character is processed immediately
onTheFlyCompress :: IO ()
onTheFlyCompress = do
  IO.hSetBuffering stdin IO.NoBuffering
  let stream = Q.stdin
             & compress
             & S.map (\n -> "\t" ++ show n)
             & S.stdoutLn
  
  evalStateT stream initEncTable


-- | Decompresses codes on arrival from stdin. For demonstration purposes. Note
-- that setting 'IO.LineBuffering' ensures you can write in codes that are long
-- enough.
onTheFlyDecompress :: IO ()
onTheFlyDecompress = do
  IO.hSetBuffering stdin IO.LineBuffering
  let stream = S.stdinLn
             & S.read
             & decompress
             & Q.chunkMap (flip B.snoc (fromIntegral $ ord '\t'))
             & Q.stdout

  evalStateT stream initDecTable


-- | Compress from 'input' to 'output' in constant space.
compressFile :: FilePath -> FilePath -> IO ()
compressFile input output =
  withFile input  ReadMode  $ \hIn ->
  withFile output WriteMode $ \hOut -> do
    let stream = Q.fromHandle hIn
                & compress
                & decode
                & Q.toHandle hOut
    
    evalStateT stream initEncTable


-- | Decompress from 'input' to 'output' in constant space.
decompressFile :: FilePath -> FilePath -> IO ()
decompressFile input output = 
  withFile input  ReadMode  $ \hIn ->
  withFile output WriteMode $ \hOut -> do
    let stream = Q.fromHandle hIn
               & encode
               & decompress
               & Q.toHandle hOut
    
    evalStateT stream initDecTable
