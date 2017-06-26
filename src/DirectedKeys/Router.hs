{-|
Module      : DirectedKeys.Router
Description : Short description
Copyright   : (c) Plow Technologies, 2017
License     : BSD3
Maintainer  : jeremy.peterson@plowtech.net
Stability   : beta

A Set of RouteBound Doubles constitutes the lookup table, care is made to
the min value in the set defines the root. Then every element that is inserted 
represents the Max value
that should return the destination.

so given Keys,

10, 15, 20, 30

and corresponding destinations of
2233, 2211, 1111, 4432

a table is built that declares
<= 10 will go to dest 2233
> 10 && <= 15 will go to dest 2211
> 15 && <= 20 will go to dest 1111
> 20 && <= 30 will go to dest 2211
> 30  will go to dest 2211

-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DirectedKeys.Router (
    KeyRouteBoundDouble(..)
  , KeyRouteLookupTable(..)
  , fromList
  , fromListWithMap
  , makeDestFcn
) where 

import Control.Applicative
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import GHC.Generics
import Prelude hiding (maxBound)


-- | 
data KeyRouteBoundDouble a b 
  = KeyRouteBoundDouble 
    { rbtMax  :: a 
    , rbtDest :: b
    } deriving (Eq,Show,Ord,Generic)

data KeyRouteLookupTable a b 
  = KeyRouteLookupTable 
    { getSet :: S.Set (KeyRouteBoundDouble a b)
    , getLookupMap :: M.Map a b
    }

-- | Build a `KeyRouteLookupTable` from a list
fromList :: (Ord a, Ord b) => [(a,b)] -> (KeyRouteLookupTable a b)
fromList lst = 
  KeyRouteLookupTable 
    { getSet = S.fromList 
      (
        ( \(maxBound, targetDestination) ->
            KeyRouteBoundDouble 
              { rbtMax  = maxBound
              , rbtDest = targetDestination 
              }
        ) <$> lst
      )
    , getLookupMap = M.empty
    }

-- | Build a `KeyRouteLookupTable` from a list and a Map
fromListWithMap :: (Ord a, Ord b) 
  => [(a,b)] 
  -> M.Map a b 
  -> (KeyRouteLookupTable a b)
fromListWithMap lst m = 
  KeyRouteLookupTable 
    { getSet = S.fromList 
      (
        ( \(maxBound, targetDestination) ->
            KeyRouteBoundDouble 
              { rbtMax  = maxBound
              , rbtDest = targetDestination 
              }
        ) <$> lst
      )
    , getLookupMap = m
    }

-- | Make a destination calculation function from a 'KeyRouteLookupTable'.
makeDestFcn :: (Ord a, Ord b, Monoid b) 
  => KeyRouteLookupTable a b 
  -> (a -> (Maybe b))
makeDestFcn krlt incoming = 
  case M.lookup incoming kbMap of
    Just res -> Just res
    Nothing  -> findMaxBound $ dummyI
  where
    findMaxBound i = rbtDest <$> (S.lookupGE i kbSet)
    kbMap = getLookupMap krlt
    kbSet = getSet krlt
    dummyI = KeyRouteBoundDouble {rbtMax = incoming, rbtDest = mempty}
