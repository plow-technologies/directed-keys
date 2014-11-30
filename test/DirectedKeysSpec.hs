{-# LANGUAGE DeriveGeneric, OverloadedStrings, NoMonomorphismRestriction #-}
module DirectedKeysSpec (main, spec) where
import DirectedKeys.Types
import GHC.Generics
import Data.Serialize
import Control.Applicative ((<$>))
import Test.Serial
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import DirectedKeys
import Test.QuickCheck
import Test.Hspec
import Data.List

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "encodeKeyRaw" $ do
    it "should box the incoming source and dest into a datatype" $ do
      let er = encodeKeyRaw a b c d
          a = exampleKey 
          b = exampleHost1 
          c = exampleHost2 
          d = exampleDate 
      (er == exampleDirectedKey) `shouldBe` True

  describe "encodeKey" $ do
    it "turn the given Key parameters into a Serialized and compressed DirectedKey" $ do
      testEncodeKey `shouldBe` "AAAAAAAAAEkfiwgAAAAAAAAD0zhSJHBtgYUdAwQIGloa6RmaWegZ6hkbWlkYWBhAJQQMDfTMDPUsjPQsLMDiGUvnzja584UTAP74XXJBAAAA" 
  describe "testSerializationIsStableOverTime" $ do 
    it "should return the same serializaiton over time" $ do rslt <- testSerializationIsStableOverTime 
                                                             ((== exampleDirectedKey) <$> rslt) `shouldBe` (Right True)


  describe "decodeKey" $ do
    it "decode the Key into it's constituent parts" $ do
      (testDecodeKey == (Right exampleDirectedKey )) `shouldBe` True
    it "should go back and forth between decode and encode for all inputs" $ do
      property makeQCDirectedKey
  describe "decodeFilename" $ do
    it "should encode and decode the filename and be the same for a complicated key" $ do
      let keys = ["!@#$%^&*(#$%^&*<><><>$$$h$$g$G$g$$$$$$$$$","$$$", "$$", "<<<<<", ">>>>>>>", "<><><<<<<>>><<<><><><>>>>", "", (C.unwords . (fmap (C.singleton . fst)) . M.toList $ toEscapedCharacters)]
      (fmap (decodeFilename . parseFilename) keys) `shouldBe` keys
  -- describe "migrateKey" $ do
  --   it "should replace the source with the old dest and then replace the dest with a new dest" $ do  
  --     True `shouldBe` False


newtype TestKey = TKey Int         deriving (Eq,Ord,Generic) 
newtype TestHost = Host1 String    deriving (Eq,Ord,Generic)      
newtype TestHost2 = Host2 String   deriving (Eq,Ord,Generic,Show)       
newtype InitDate = IDate Int       deriving (Eq,Ord,Generic)  


makeQCDirectedKey :: Int -> Int -> Int -> Int  -> Bool 
makeQCDirectedKey i j k l = let ky = TKey ( i ) 
                                s = Host1 (show j) 
                                s2 = Host2(show k) 
                                dt = IDate (l)
                                dkr = DKeyRaw ky s s2 dt 
                            in (decodeKey.encodeKey $ dkr) == (Right dkr )
    

 
makeSimpleDuplicatesTestDirectedKey = let s = Host1 (show "www.aacs-us.com") 
                                          s2 = Host2(show "www.aacs-us.com") 
                                          dt = IDate (3)
                                          dkr ky = DKeyRaw (TKey  ky )  s s2 dt
                                          dkrslt = fmap (encodeKey.dkr)  [1 .. 200000]
                                      in (filter (\lst -> length lst > 1)).group $ dkrslt
                               
                               



instance Serialize TestKey where
instance Serialize TestHost where
instance Serialize TestHost2 where
instance Serialize InitDate where


exampleHost1 :: TestHost
exampleHost1 = Host1 "192.168.1.31:8080"  


exampleKey :: TestKey 
exampleKey = TKey 2937598273598273598

exampleHost2 :: TestHost2
exampleHost2 = Host2 "10.61.82.88:8080" 

exampleDate :: InitDate
exampleDate = IDate 81327582735872357385

exampleDirectedKey :: DirectedKeyRaw TestKey TestHost TestHost2 InitDate
exampleDirectedKey = DKeyRaw { 
                       getSimpleKey = exampleKey , 
                       getSource    = exampleHost1,
                       getDest      = exampleHost2 ,
                       getDateTime  = exampleDate 
                     } 

testEncodeKey :: BS.ByteString 
testEncodeKey = encodeKey exampleDirectedKey

testEncodeKeyParg :: BS.ByteString 
testEncodeKeyParg = encodeKeyPart exampleDirectedKey


testDecodeKey :: (Either String (DirectedKeyRaw TestKey TestHost TestHost2 InitDate ))
testDecodeKey = decodeKey "AAAAAAAAAEkfiwgAAAAAAAAD0zhSJHBtgYUdAwQIGloa6RmaWegZ6hkbWlkYWBhAJQQMDfTMDPUsjPQsLMDiGUvnzja584UTAP74XXJBAAAA" -- old test string "AAAAAAAAAEgfiwgAAAAAAAAD0zhSJHBtgYUdAwSIlJeX6yUmJhfrlhbrJefnWlkYWBhA5YTBcrmJVfl5cKmMpXNnm9z5wgkAlPPJ7kcAAAA="

testSerializationIsStableOverTime
  :: IO
       (Either
          TestError (DirectedKeyRaw TestKey TestHost TestHost2 InitDate))
testSerializationIsStableOverTime = runCerealSerializationTest exampleDirectedKey "serialize_stability_test"

{-|






encodedKey :: Bytestring
encodedKey = encodeKey exampleDirectedKey

encodedKeyRaw :: Bytestring
encodedKeyRaw = encodeKeyRaw exampleDirectedKey


decodedKey :: DirectedKey MongoDBHost AlarmId TachHost
decodedKey = decodeKey encodedKey 

decodedKeyCompressed :: DirectedKey MongoDBHost AlarmId TachHost
decodedKeyCompressed  = decodeKeyRaw encodedKeyRaw

migratedKey :: DirectedKey TachHost AlarmId TachHost
migratedKey = migrateKey encodedKey exampleNewDest
|-}
