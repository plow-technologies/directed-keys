{-|
Module      : DirectedKeys
Description : Short description
Copyright   : (c) Plow Technologies, 2017
License     : BSD3
Maintainer  : jeremy.peterson@plowtech.net
Stability   : beta

-}

{-# LANGUAGE OverloadedStrings #-}

module DirectedKeys (
    encodeKey
  , encodeKeyPart
  , decodeKey 
  , decodeKeyPart
  , toEscapedCharacters
  , parseFilename
  , decodeFilename
) where

import Codec.Compression.GZip
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Base64.URL as BS_Url
import qualified Data.Map.Strict            as M
import qualified Data.Serialize             as S
import DirectedKeys.Types


-- | Encode a 'DirectedKeyRaw' without converting to Base64.
encodeKeyPart :: (S.Serialize key, S.Serialize source, S.Serialize destination,  S.Serialize datetime) 
  => DirectedKeyRaw key source destination datetime 
  -> BS.ByteString
encodeKeyPart dKeyRaw = 
  S.runPut $ S.put . DK . BSL.toStrict . compress. S.runPutLazy. S.put $ dKeyRaw



-- | Encode a 'DirectedKeyRaw' then convert the result to Base64.
encodeKey :: (S.Serialize key, S.Serialize source, S.Serialize destination,  S.Serialize datetime) 
  => DirectedKeyRaw key source destination datetime
  -> BS.ByteString
encodeKey dKeyRaw = 
   BS_Url.encode . S.runPut $ S.put . DK . BSL.toStrict . compress . S.runPutLazy . S.put $ dKeyRaw


-- | Try to decode the 'ByteString' of a encoded 'DirectedKeyRaw' which was 
-- not converted to Base64.
decodeKeyPart :: (S.Serialize key, S.Serialize source, S.Serialize destination, S.Serialize datetime) 
  => BS.ByteString 
  -> Either String (DirectedKeyRaw key source destination datetime)
decodeKeyPart dkbs = do
  dk <- S.runGet dkGet dkbs
  S.runGet S.get $ BSL.toStrict.decompress . getDKLazy $ dk

-- | Try to decode the 'ByteString' of a encoded 'DirectedKeyRaw' which was 
-- converted to Base64.
decodeKey :: (S.Serialize key, S.Serialize source, S.Serialize destination,  S.Serialize datetime) 
  => BS.ByteString 
  -> Either String (DirectedKeyRaw key source destination datetime )
decodeKey bs = do
  dkbs <- BS_Url.decode bs
  dk <- S.runGet dkGet dkbs
  S.runGet S.get $ BSL.toStrict.decompress . getDKLazy $ dk
      

-- | deserialize 'DirectedKey' ByteString
dkGet :: S.Get DirectedKey
dkGet = S.get :: S.Get DirectedKey

-- | Convert strict 'B.ByteString' serialization of 'DirectedKeyRaw' to a lazy 
-- 'BLS.ByteString'
getDKLazy :: DirectedKey -> BSL.ByteString
getDKLazy dk = BSL.fromStrict . getDKString $ dk

-- | Create a filename from a 'ByteString'. It escapes all invalid unix filename 
-- characters.
parseFilename :: C.ByteString -> C.ByteString
parseFilename  = C.foldl' replaceInMap C.empty

-- | something
replaceInMap :: C.ByteString -> Char -> C.ByteString
replaceInMap accum c =
  case mVal of
    Nothing -> C.append accum (C.singleton c)
    Just val -> C.append accum val
  where
    mVal = M.lookup c toEscapedCharacters

-- | Convert a escaped 'Bytestring' to an unescaped 'ByteString'.
decodeFilename :: C.ByteString -> C.ByteString
decodeFilename input
  | C.null input = C.empty
  | otherwise = finish f1 f2
      where 
        (f1,f2) = C.foldl' replaceFromMap (C.singleton . C.head $ input, C.empty) (C.tail input)
        finish f f' 
          | C.length f == 2 = C.append f' $ maybe f (\x -> C.singleton x) $ M.lookup f fromEscapedCharacters  
          | otherwise = C.append f' f 

-- | Look up two sequential 'Char's. If they are escaped, then replace them with 
-- the unescaped version.
replaceFromMap :: 
     (C.ByteString, C.ByteString) -- ^ the last character of the 'B.ByteString' that was seen and the accumulator.
  -> Char                         -- ^ newest character of the 'B.ByteString'.
  -> (C.ByteString ,C.ByteString)
replaceFromMap (lookupString, accum) c 
  | C.null lookupString        = (C.singleton c, accum)
  | C.length lookupString == 2 = 
      let mVal = M.lookup lookupString fromEscapedCharacters 
      in case mVal of
           Nothing  -> ( C.append (C.tail lookupString) (C.singleton c)
                       , (C.append accum (C.init lookupString))
                       )
           Just val -> ( C.singleton c
                       , (C.append accum (C.singleton val))
                       )
  | otherwise = (C.append lookupString (C.singleton c), accum)

-- | Escape a character that is invalid as a Unix Filename.
toEscapedCharacters :: M.Map Char C.ByteString
toEscapedCharacters = 
  M.fromList 
    [ ('$', "$$")
    , ('?', "$q")
    , ('>', "$g")
    , ('<', "$l")
    , ('%', "$p")
    , ('*', "$a")
    , (':', "$c")
    , ('|', "$d")
    , ('\\', "$b")
    , ('/', "$f")
    , ('|', "$i")
    ]

-- | Get the original character from an escaped char from a Unix Filename.
fromEscapedCharacters :: M.Map C.ByteString Char
fromEscapedCharacters = reverseMap toEscapedCharacters

-- | Flip the keys ('Char') and values ('C.ByteString') of a 'M.Map'.
reverseMap :: M.Map Char C.ByteString -> M.Map C.ByteString Char
reverseMap m = M.fromList [(b,a) | (a,b) <- M.toList m]
