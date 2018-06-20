{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, RecordWildCards, DeriveGeneric #-}
module DirectedKeys.Types (DirectedKeyRaw(..)
                         , DirectedKey (..) 
) where 

import Prelude (Eq, Ord) 
import GHC.Generics 
import Data.Serialize
import Data.ByteString
import Data.Hashable
import Control.DeepSeq (NFData)

data DirectedKeyRaw skey src dst datetime = DKeyRaw { 
      getSimpleKey :: !skey      
      ,getSource   :: !src    
      ,getDest     :: !dst 
      ,getDateTime :: !datetime     

} deriving (Eq,Ord,Generic)

-- | This instance defines how the data becomes serialized
instance (Serialize a, Serialize b, Serialize c, Serialize d) => Serialize (DirectedKeyRaw a b c d)

instance (Hashable a,Hashable b, Hashable c, Hashable d) => Hashable (DirectedKeyRaw a b c d)

instance (NFData a, NFData b, NFData c, NFData d) => NFData (DirectedKeyRaw a b c d)


newtype DirectedKey = DK {getDKString :: ByteString} 
  deriving (Eq,Ord,Generic) 

instance Serialize DirectedKey

instance Hashable DirectedKey

instance NFData DirectedKey
