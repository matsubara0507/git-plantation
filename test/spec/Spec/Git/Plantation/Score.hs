{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Spec.Git.Plantation.Score (spec) where

import           RIO                  hiding (link2)


import           Data.Extensible
import qualified Data.IntMap.Strict   as IntMap
import qualified Fixture
import           Git.Plantation.Score (mkScore)
import           Git.Plantation.Store (Store)
import qualified Git.Plantation.Store as Store
import           Test.Tasty.Hspec

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
                <: #answerer     @= Nothing
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
      mkScore (Fixture.config ^. #problems) store team `shouldBe` expect

store :: Store
store = IntMap.fromList
    [ (1, [ build1 ])
    , (2, [ build2 ])
    , (3, [])
    ]

build1 :: Store.Build
build1
    = #source      @= "alpha"
   <: #status      @= "success"
   <: #source_repo @= ""
   <: #created     @= 1560000000
   <: #message     @= "pushed by: @matsubara0507"
   <: nil

build2 :: Store.Build
build2
    = #source      @= "alpha"
   <: #status      @= "pending"
   <: #source_repo @= ""
   <: #created     @= 1560000100
   <: #message     @= "pushed by: @"
   <: nil
