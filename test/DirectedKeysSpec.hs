module DirectedKeysSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "encodeKey" $ do
    it "should encode the key in a retrievable Bytestring" $ do
      True `shouldBe` False
  describe "encodeKeyRaw" $ do
    it "should encode the key without compression" $ do
      True `shouldBe` False
  describe "decodeKeyRaw" $ do
    it "decode the Raw Key into it's constituent parts" $ do
      True `shouldBe` False
  describe "decodeKey" $ do
    it "decode the Key into it's constituent parts" $ do
      True `shouldBe` False
  describe "migrateKey" $ do
    it "should replace the source with the old dest and then replace the dest with a new dest" $ do  
      True `shouldBe` False





import DirectedKeys



-- |Instances Ord, Eq, Generic


type Entity Alarm = Entity (AlarmId) Alarm 
    

type MongoDBHost = Text 

type TachHost = Text 

type InitDaate = Int64

exampleAlarm = Entity "oAAbxkd284878" (Alarm {..} ) 

exampleMongoHost = "10.61.131.97:3036"
exampleDest = "www.amazon.com/aws/someS3Bucket"

exampleNewDest = "192.168.1.43:3032"

exampleTime = 82348572766


exampleDirectedKey = DKey { 
                       getSimpleKey = entityKey exampleAlarm , 
                       getSource    = exampleMongoHost,
                       getDest      = exampleDest ,
                       getDateTime  = exampleTime 
                     } 


encodedKey :: Bytestring
encodedKey = encodeKey exampleDirectedKey

encodedKeyRaw :: Bytestring
encodedKeyRaw = encodeKeyRaw exampleDirectedKey


decodedKey :: DirectedKey MongoDBHost AlarmId TachHost
decodedKey = decodeKey encodedKey 

decodedKeyCompressed :: DirectedKey MongoDBHost AlarmId TachHost
decodedKeyCompressed  = decodeKeyRaw encodedKeyRaw



migratedKey = Bytestring
migratedKey = migrateKey encodedKey exampleNewDest
