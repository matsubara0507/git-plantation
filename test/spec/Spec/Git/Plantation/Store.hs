{-# LANGUAGE OverloadedLabels #-}

module Spec.Git.Plantation.Store (spec) where

import           RIO                  hiding (link2)


import           Data.Extensible
import qualified Git.Plantation.Store as Store
import           Test.Tasty.Hspec

spec :: Spec
spec = do
  describe "isCorrect" $ do
    context "when build status is pending" $ do
      let build' = build1 & #status `set` "pending"
      it "should return False" $
        Store.isCorrect build' `shouldBe` False
    context "when build status is running" $ do
      let build' = build1 & #status `set` "running"
      it "should return False" $
        Store.isCorrect build' `shouldBe` False
    context "when build status is success" $ do
      let build' = build1 & #status `set` "success"
      it "should return True" $
        Store.isCorrect build' `shouldBe` True
    context "when build status is failure" $ do
      let build' = build1 & #status `set` "failure"
      it "should return False" $
        Store.isCorrect build' `shouldBe` False

  describe "isPending" $ do
    context "when build status is pending" $ do
      let build' = build1 & #status `set` "pending"
      it "should return True" $
        Store.isPending build' `shouldBe` True
    context "when build status is running" $ do
      let build' = build1 & #status `set` "running"
      it "should return True" $
        Store.isPending build' `shouldBe` True
    context "when build status is success" $ do
      let build' = build1 & #status `set` "success"
      it "should return False" $
        Store.isPending build' `shouldBe` False
    context "when build status is failure" $ do
      let build' = build1 & #status `set` "failure"
      it "should return False" $
        Store.isPending build' `shouldBe` False

  describe "uniqByTeam" $ do
    it "should return builds uniqued by team" $
      Store.uniqByTeam [build1, build2, build3, build4] `shouldBe` [build1, build2]
    context "when contain two success biuld" $
      it "should return fast build" $ do
        let build1' = build1 & #created `set` 1560100000
        Store.uniqByTeam [build1', build1] `shouldBe` [build1]

  describe "uniqByPlayer" $ do
    it "should return builds uniqued by team" $
      Store.uniqByPlayer [build1, build2, build3, build4] `shouldBe` [build1, build2]
    context "when contain two success biuld" $
      it "should return fast build" $ do
        let build1' = build1 & #created `set` 1560100000
        Store.uniqByPlayer [build1', build1] `shouldBe` [build1]
    context "when contain different players" $
      it "should return two builds" $ do
        let build1' = build1 & #created `set` 1560100000 & #message `set` "pushed by: @octcat"
        Store.uniqByPlayer [build1', build1] `shouldBe` [build1, build1']

build1 :: Store.Build
build1
    = #source @= "alpha"
   <: #status @= "success"
   <: #source_repo @= ""
   <: #created @= 1560000000
   <: #message @= "pushed by: @matsubara0507"
   <: nil

build2 :: Store.Build
build2
    = #source @= "bravo"
   <: #status @= "success"
   <: #source_repo @= ""
   <: #created @= 1560002000
   <: #message @= "pushed by: @octcat"
   <: nil

build3 :: Store.Build
build3
    = #source @= "bravo"
   <: #status @= "failure"
   <: #source_repo @= ""
   <: #created @= 1560001000
   <: #message @= "pushed by: @octcat"
   <: nil

build4 :: Store.Build
build4
    = #source @= "bravo"
   <: #status @= "pending"
   <: #source_repo @= ""
   <: #created @= 1560003000
   <: #message @= "pushed by: @octcat"
   <: nil
