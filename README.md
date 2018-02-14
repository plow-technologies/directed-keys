
# directed-keys

Directed Keys consists of two parts, first a set of functions for encoding base-64 encoded values that can be used as keys.
Second, a destination selector that takes a list of max bound and destinations... returns a destination for any key in these ranges

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Usage

``` haskell

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


```
