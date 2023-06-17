{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Git.Plantation.Score (spec) where

import           RIO                     hiding (link2)

import           Data.Extensible
import qualified Fixture
import           Git.Plantation.Data.Job (Job)
import           Git.Plantation.Score    (mkScore)
import           Test.Hspec

spec :: Spec
spec = do
  describe "mkScore" $ do
    it "should make Score" $ do
      let [team] = Fixture.config ^. #teams
          stat1  = #problem_id   @= 1
                <: #correct      @= True
                <: #pending      @= False
                <: #corrected_at @= Just 1560000000
                <: #answerer     @= Just "matsubara0507"
                <: nil
          stat2  = #problem_id   @= 2
                <: #correct      @= False
                <: #pending      @= True
                <: #corrected_at @= Nothing
                <: #answerer     @= Just "matsubara0507"
                <: nil
          link1  = #problem_id @= 1
                <: #url        @= "https://github.com/sample-hige/git-challenge-tutorial"
                <: nil
          link2  = #problem_id @= 2
                <: #url        @= "https://github.com/sample-hige/git-challenge-is-order-an-adding"
                <: nil
          link3  = #problem_id @= 3
                <: #url        @= "https://github.com/sample-hige/git-challenge-minesweeper"
                <: nil
          expect = #team  @= "alpha"
                <: #point @= 1
                <: #stats @= [stat1, stat2]
                <: #links @= [link1, link2, link3]
                <: nil
      mkScore (Fixture.config ^. #problems) team [job1, job2] `shouldBe` expect

job1, job2 :: Job
job1
    = #id      @= 1
   <: #problem @= 1
   <: #team    @= "alpha"
   <: #author  @= Just "matsubara0507"
   <: #queuing @= False
   <: #running @= False
   <: #success @= True
   <: #stdout  @= ""
   <: #stderr  @= ""
   <: #created @= 1560000000
   <: nil
job2
    = #id      @= 2
   <: #problem @= 2
   <: #team    @= "alpha"
   <: #author  @= Just "matsubara0507"
   <: #queuing @= False
   <: #running @= True
   <: #success @= False
   <: #stdout  @= ""
   <: #stderr  @= ""
   <: #created @= 1560000000
   <: nil
