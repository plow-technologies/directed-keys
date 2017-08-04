{-# LANGUAGE OverloadedStrings #-}

module DirectedKeys.RouterSpec ( 
    main 
  , spec
  ) where 

import Data.Maybe
import DirectedKeys.Router
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fromList" $ do
    it "should take a list of some orderable partial key and return a dest" $ do
      let md1 = testDestFcn 1
          md2 = testDestFcn 11
          md3 = testDestFcn 21
          md4 = testDestFcn 31
          mds = catMaybes $ [md1,md2,md3,md4]
          tgts = (snd <$> testKeyList)
          rslt = (tgts == mds)
      True `shouldBe` rslt


testKeyList :: [(Int,String)]
testKeyList = [ (10,"www.aacs-us.com:2233")
              , (20,"www.aacs-us.com:2234")
              , (30,"www.aacs-us.com:2235")
              , (40,"www.aacs-us.com:2236")]

testDestFcn :: Int -> Maybe String
testDestFcn = 
  let krlt = (fromList testKeyList)
  in  makeDestFcn krlt 
