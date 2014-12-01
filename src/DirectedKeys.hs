{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

module DirectedKeys (
encodeKeyRaw
,encodeKey
,encodeKeyPart
,decodeKey 
,decodeKeyPart
,toEscapedCharacters
,parseFilename
,decodeFilename
) where

import qualified Data.Serialize as S
import DirectedKeys.Types
import Codec.Compression.GZip
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as BS_Url
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as C


encodeKeyRaw :: ( S.Serialize key, S.Serialize source, S.Serialize destination, S.Serialize datetime) => 
                key -> 
                source -> 
                destination -> 
                datetime -> 
                DirectedKeyRaw key source destination datetime
encodeKeyRaw = DKeyRaw  


-- encode without converting to base 64
encodeKeyPart :: (S.Serialize key, S.Serialize source, S.Serialize destination,  S.Serialize datetime) =>  
                 DirectedKeyRaw key source destination datetime -> BS.ByteString
encodeKeyPart (DKeyRaw k s d dt) = S.runPut $ S.put . DK . BSL.toStrict .  compress. S.runPutLazy. S.put $ encodeKeyRaw k s d dt



-- Make into base 64
encodeKey :: (S.Serialize key, S.Serialize source, S.Serialize destination,  S.Serialize datetime) =>  
             DirectedKeyRaw key source destination datetime -> BS.ByteString
encodeKey (DKeyRaw k s d dt) = BS_Url.encode . S.runPut $ S.put . DK . BSL.toStrict .  compress. S.runPutLazy. S.put $ encodeKeyRaw k s d dt



decodeKeyPart :: (S.Serialize key, S.Serialize source, S.Serialize destination,  S.Serialize datetime) => BS.ByteString -> 
                 Either String (DirectedKeyRaw key source destination datetime )
decodeKeyPart dkbs = do
  dk <- S.runGet dkGet dkbs
  S.runGet S.get $ BSL.toStrict.decompress . getDKLazy $ dk


decodeKey :: (S.Serialize key, S.Serialize source, S.Serialize destination,  S.Serialize datetime) => BS.ByteString -> 
             Either String (DirectedKeyRaw key source destination datetime )
decodeKey bs = do
  dkbs <- BS_Url.decode bs
  dk <- S.runGet dkGet dkbs
  S.runGet S.get $ BSL.toStrict.decompress . getDKLazy $ dk
      

dkGet :: S.Get DirectedKey
dkGet = S.get :: S.Get DirectedKey

getDKLazy :: DirectedKey -> BSL.ByteString
getDKLazy dk = BSL.fromStrict . getDKString $ dk


-- | Use this function to create a filename from a bytestring, it escapes all invalid unix filename characters
parseFilename :: C.ByteString -> C.ByteString
parseFilename  = C.foldl' replaceInMap C.empty

replaceInMap :: C.ByteString -> Char -> C.ByteString
replaceInMap accum c =
  case mVal of
    Nothing -> C.append accum (C.singleton c)
    Just val -> C.append accum val
  where
    mVal = M.lookup c toEscapedCharacters



-- | decodeFilename  returns a Bytestring that has unescaped all the characters for filenames
decodeFilename :: C.ByteString -> C.ByteString
decodeFilename input
  | C.null input = C.empty
  | otherwise = finish f1 f2
      where 
        (f1,f2) = C.foldl' replaceFromMap (C.singleton . C.head $ input, C.empty) (C.tail input)
        finish f f' 
          | C.length f == 2 = C.append f' $ maybe f (\x -> C.singleton x) $ M.lookup f fromEscapedCharacters  
          | otherwise = C.append f' f 


replaceFromMap :: (C.ByteString, C.ByteString) -- the last character of the bytestring that was seen and the accumulator
                   -> Char             -- newest character of the bytestring
                   -> (C.ByteString ,C.ByteString)
replaceFromMap (lookupString, accum) c 
  |C.null lookupString = (C.singleton c, accum)
  |C.length lookupString == 2 = let mVal = M.lookup lookupString fromEscapedCharacters 
                                in case mVal of
                                     Nothing -> (C.append (C.tail lookupString) (C.singleton c), (C.append accum (C.init lookupString)))
                                     Just val -> (C.singleton c , (C.append accum (C.singleton val)))              
  |otherwise = (C.append lookupString (C.singleton c), accum)

toEscapedCharacters :: M.Map Char C.ByteString
toEscapedCharacters = M.fromList [('$', "$$"),('?', "$q"),('>', "$g"),('<', "$l"), ('%',"$p"),('*',"$a"),(':',"$c"),('|',"$d"),('\\', "$b"),('/',"$f"), ('|',"$i")]

fromEscapedCharacters :: M.Map C.ByteString Char
fromEscapedCharacters = reverseMap toEscapedCharacters


reverseMap :: M.Map Char C.ByteString -> M.Map C.ByteString Char
reverseMap m = M.fromList [(b,a) | (a,b) <- M.toList m]
