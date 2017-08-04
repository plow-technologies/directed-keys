{-|
Module      : DirectedKeys.Types
Description : Short description
Copyright   : (c) Plow Technologies, 2017
License     : BSD3
Maintainer  : jeremy.peterson@plowtech.net
Stability   : beta

-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE RecordWildCards   #-} 

module DirectedKeys.Types (
    DirectedKeyRaw (..)
  , DirectedKey (..) 
  ) where 

import Data.ByteString
import Data.Hashable
import Data.Serialize
import GHC.Generics 
import Prelude (Eq, Ord) 

-- | Data type to hold a generic grouping of a key, data source, destination 
-- source and time.
data DirectedKeyRaw skey src dst datetime = 
  DKeyRaw 
    { getSimpleKey :: !skey      
    , getSource    :: !src    
    , getDest      :: !dst 
    , getDateTime  :: !datetime     
    } deriving (Eq,Ord,Generic) 

instance (Serialize a, Serialize b, Serialize c, Serialize d) 
  => Serialize (DirectedKeyRaw a b c d)

instance (Hashable a, Hashable b, Hashable c, Hashable d) 
  => Hashable (DirectedKeyRaw a b c d)

-- | Stores the value of 'DirectedKeyRaw' as a 'ByteString'.
newtype DirectedKey = DK 
  { getDKString :: ByteString
  } deriving (Eq,Ord,Generic) 

instance Serialize DirectedKey

instance Hashable DirectedKey
