{-# LANGUAGE DeriveGeneric, NoMonomorphismRestriction, RecordWildCards #-}

module DirectedKeys (
encodeKeyRaw
,encodeKey
,encodeKeyPart

,decodeKey 
,decodeKeyPart
) where

import qualified Data.Serialize as S
import DirectedKeys.Types
import Codec.Compression.GZip
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as BS_Url
--import Data.Compressed.LZ78
-- import DirectedKeys.Internal


encodeKeyRaw :: ( S.Serialize key, S.Serialize source, S.Serialize destination, S.Serialize datetime) => 
                key -> 
                source -> 
                destination -> 
                datetime -> 
                DirectedKeyRaw key source destination datetime
encodeKeyRaw key source destination datetime = DKeyRaw  key source destination datetime



encodeKeyPart :: (S.Serialize key, S.Serialize source, S.Serialize destination,  S.Serialize datetime) =>  (DirectedKeyRaw key source destination datetime) -> BS.ByteString
encodeKeyPart (DKeyRaw k s d dt) = S.runPut $ S.put . DK . BSL.toStrict .  compress. S.runPutLazy. S.put $ encodeKeyRaw k s d dt


encodeKey :: (S.Serialize key, S.Serialize source, S.Serialize destination,  S.Serialize datetime) =>  (DirectedKeyRaw key source destination datetime) -> BS.ByteString
encodeKey (DKeyRaw k s d dt) = BS_Url.encode . S.runPut $ S.put . DK . BSL.toStrict .  compress. S.runPutLazy. S.put $ encodeKeyRaw k s d dt



decodeKeyPart :: (S.Serialize key, S.Serialize source, S.Serialize destination,  S.Serialize datetime) => BS.ByteString -> (Either String (DirectedKeyRaw key source destination datetime ))
decodeKeyPart dkbs = do
  dk <- (S.runGet dkGet dkbs)
  (S.runGet S.get $ BSL.toStrict.decompress . getDKLazy $ dk)
    where
      dkGet = S.get :: S.Get DirectedKey
      getDKLazy dk = (BSL.fromStrict . getDKString $ dk)  




decodeKey :: (S.Serialize key, S.Serialize source, S.Serialize destination,  S.Serialize datetime) => BS.ByteString -> (Either String (DirectedKeyRaw key source destination datetime ))
decodeKey bs = do
  dkbs <- BS_Url.decode bs
  dk <- (S.runGet dkGet dkbs)
  (S.runGet S.get $ BSL.toStrict.decompress . getDKLazy $ dk)
    where
      dkGet = S.get :: S.Get DirectedKey
      getDKLazy dk = (BSL.fromStrict . getDKString $ dk)  
