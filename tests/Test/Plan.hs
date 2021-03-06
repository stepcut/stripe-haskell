{-# LANGUAGE OverloadedStrings #-}
module Test.Plan where

import           Control.Monad (void)
import           Data.Either   (isRight)

import           Test.Hspec
import           Test.Util     (makePlanId)

import           Web.Stripe
import           Web.Stripe.Plan

planTests :: StripeConfig -> Spec
planTests config = do
  describe "Plan tests" $ do
    it "Succesfully creates a Plan" $ do
      planid <- makePlanId
      result <- stripe config $ do
        p <- createPlan planid
                        0 -- free plan
                        USD
                        Month
                        "sample plan"
                        []
        void $ deletePlan planid
        return p
      result `shouldSatisfy` isRight
    it "Succesfully deletes a Plan" $ do
      planid <- makePlanId
      result <- stripe config $ do
        Plan { planId = pid } <-
          createPlan planid
          0 -- free plan
          USD
          Month
          "sample plan"
          []
        deletePlan pid
      result `shouldSatisfy` isRight
    it "Succesfully updates a Plan" $ do
      planid <- makePlanId
      result <- stripe config $ do
        Plan { planId = pid } <-
          createPlan planid
          0 -- free plan
          USD
          Month
          "sample plan"
          []
        r <- updatePlanBase pid (Just "cookie")
                                (Just "test")
                                [("key","value")]
        void $ deletePlan pid
        return r
      result `shouldSatisfy` isRight
      let Right Plan { planMetaData = pm
                     , planName = pname
                     , planDescription = pdesc
                     } = result
      pm `shouldBe` [("key", "value")]
      pname `shouldBe` "cookie"
      pdesc `shouldBe` Just "test"

    it "Succesfully retrieves a Plan" $ do
      planid <- makePlanId
      result <- stripe config $ do
        Plan { planId = pid } <-
          createPlan planid
          0 -- free plan
          USD
          Month
          "sample plan"
          []
        r <- getPlan pid
        void $ deletePlan pid
        return r
      result `shouldSatisfy` isRight
    it "Succesfully retrieves a list of Plans" $ do
      result <- stripe config $ getPlans Nothing Nothing Nothing
      result `shouldSatisfy` isRight




