{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, RecordWildCards, DeriveGeneric #-}
module DirectedKeys.Router (
  fromList
  , makeDestFcn
  , KeyRouteLookupTable
) where 


import Prelude (Eq, Ord,Show,undefined,($),Maybe)
import Data.Monoid 
import Control.Applicative
import GHC.Generics
import Data.Serialize
import qualified Data.Set as S


{-|

A Set of RouteBound Doubles constitutes the lookup table, care is made to
the min value in the set defines the root. Then every element that is inserted represents the Max value
that should return the  destination.

so given Keys,

10, 15, 20, 30

and corresponding destinations of
2233, 2211,1111,4432

a table is built that declares
<= 10 will go to dest 2233
> 10 && <= 15 will go to dest 2211
> 15 && <= 20 will go to dest 1111
> 20 && <= 30 will go to dest 2211
> 30  will go to dest 2211


|-}


data KeyRouteBoundDouble a b = KeyRouteBoundDouble { rbtMax :: a , rbtDest :: b}
 deriving (Eq,Show,Ord,Generic)


data KeyRouteLookupTable a b =  KeyRouteLookupTable {
  getSet :: S.Set (KeyRouteBoundDouble a b)
  }

-- |Build a 
fromList :: (Ord a, Ord b, Monoid b) => [(a,b)] -> (KeyRouteLookupTable a b)
fromList lst = KeyRouteLookupTable { getSet = S.fromList (
                                        (\(maxBound, targetDestination) ->
                                        KeyRouteBoundDouble {rbtMax = maxBound, rbtDest=targetDestination })
                                        <$> lst)}

makeDestFcn :: (Ord a, Ord b, Monoid b) =>
               KeyRouteLookupTable a b ->
               (a ->  (Maybe b))
makeDestFcn krlt incoming = findMaxBound dummyI
  where
    findMaxBound i = rbtDest <$> (S.lookupGE i kbSet)
    kbSet = getSet krlt
    dummyI = KeyRouteBoundDouble {rbtMax=incoming, rbtDest= mempty}
