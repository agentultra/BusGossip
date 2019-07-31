import Test.Hspec

import Control.Monad.State
import Data.List.NonEmpty

import Lib

main :: IO ()
main = hspec $ do
  describe "nextStop" $ do
    it "should return the next stop" $ do
      nextStop 0 (Route $ 1 :| [2]) `shouldBe` 1

    it "should loop around to the first stop" $ do
      nextStop 1 (Route $ 1 :| [2]) `shouldBe` 0

  describe "driveToNextStop" $ do
    it "should move driver to the next stop on their route" $ do
      let d = Driver 'A' (Route $ 1 :| [2]) 0 []
          expected = d { driverStop = 1 }
      driveToNextStop d `shouldBe` expected

  describe "moveDrivers" $ do
    it "should move all drivers to the next stop on their route" $ do
      let d1 = Driver 'A' (Route $ 1 :| [2]) 0 []
          d2 = Driver 'B' (Route $ 1 :| [3]) 1 []
          initState = Sim [d1, d2] 0
          expectedState
            = Sim
              [ d1 { driverStop = 1 }
              , d2 { driverStop = 0 }
              ]
              0
      execState moveDrivers initState `shouldBe` expectedState

  describe "shareGossip" $ do
    it "should share no gossip if there is only one driver" $ do
      let drivers = [ Driver 'A' (Route $ 1 :| [2]) 0 [] ]
          expected = [ Driver 'A' (Route $ 1 :| [2]) 0 [] ]
      shareGossip drivers `shouldBe` expected

    it "should share gossip with other drivers at the same stop" $ do
      let drivers = [ Driver 'A' (Route $ 1 :| [2]) 0 []
                    , Driver 'B' (Route $ 1 :| [2]) 0 []
                    ]
          expected = [ Driver 'A' (Route $ 1 :| [2]) 0 ['B']
                     , Driver 'B' (Route $ 1 :| [2]) 0 ['A']
                     ]
      shareGossip drivers `shouldBe` expected

    it "should not share gossip with drivers at other stops" $ do
      let drivers = [ Driver 'A' (Route $ 1 :| [2]) 0 []
                    , Driver 'B' (Route $ 1 :| [2]) 1 []
                    ]
          expected = [ Driver 'A' (Route $ 1 :| [2]) 0 []
                     , Driver 'B' (Route $ 1 :| [2]) 1 []
                     ]
      shareGossip drivers `shouldBe` expected

    it "should not share gossip twice" $ do
      let drivers = [ Driver 'A' (Route $ 1 :| [2]) 0 ['B']
                    , Driver 'B' (Route $ 1 :| [2]) 0 ['A']
                    ]
          expected = [ Driver 'A' (Route $ 1 :| [2]) 0 ['B']
                     , Driver 'B' (Route $ 1 :| [2]) 0 ['A']
                     ]
      shareGossip drivers `shouldBe` expected

  describe "updateDriverGossips" $ do
    it "should share gossip with all drivers at the stop" $ do
      let d1 = Driver 'A' (Route $ 1 :| [2]) 0 []
          d2 = Driver 'B' (Route $ 1 :| [3]) 0 []
          initState = Sim [d1, d2] 0
          expectedState
            = Sim
              [ d1 { driverGossips = ['B'] }
              , d2 { driverGossips = ['A'] }
              ]
              0
      execState updateDriverGossips initState
        `shouldBe`
        expectedState

  describe "updateSim" $ do
    it "should update the entire simulation one minute" $ do
      let d1 = Driver 'A' (Route $ 1 :| [2]) 1 []
          d2 = Driver 'B' (Route $ 1 :| [2, 3]) 2 []
          initState = Sim [d1, d2] 0
          expectedState
            = Sim
            [ d1 { driverStop = 0, driverGossips = ['B'] }
            , d2 { driverStop = 0, driverGossips = ['A'] }
            ]
            1
      execState updateSim initState `shouldBe` expectedState

  describe "allGossipShared" $ do
    it "should be true when all drivers have their gossip" $ do
      let d1 = Driver 'A' (Route $ 1 :| []) 0 ['B']
          d2 = Driver 'B' (Route $ 1 :| []) 0 ['A']
      allGossipShared [d1, d2] `shouldBe` True

    it "should be false otherwise" $ do
      let d1 = Driver 'A' (Route $ 1 :| []) 0 []
          d2 = Driver 'B' (Route $ 1 :| []) 0 []
      allGossipShared [d1, d2] `shouldBe` False

    it "should also be false when some have some gossip" $ do
      let d1 = Driver 'A' (Route $ 1 :| []) 0 []
          d2 = Driver 'B' (Route $ 1 :| []) 0 ['A']
      allGossipShared [d1, d2] `shouldBe` False

    it "should be false if there's only one driver" $ do
      let d1 = Driver 'A' (Route $ 1 :| []) 0 []
      allGossipShared [d1] `shouldBe` False

    it "should be false if there are no drivers" $ do
      allGossipShared [] `shouldBe` False

  describe "runSim" $ do
    it "should return Nothing if there are no drivers" $ do
      runSim [] `shouldBe` Nothing

    it "should return Nothing if there is one driver" $ do
      runSim [Driver 'A' (Route $ 1 :| []) 0 []]
        `shouldBe`
        Nothing

    it "should return Nothing if no solution can be found" $ do
      let d1 = Driver 'A' (Route $ 1 :| []) 0 []
          d2 = Driver 'B' (Route $ 2 :| []) 0 []
      runSim [d1, d2] `shouldBe` Nothing

    it "should return a solution if one exists" $ do
      let d1 = Driver 'A' (Route $ 1 :| [2]) 0 []
          d2 = Driver 'B' (Route $ 2 :| [3, 4, 2]) 0 []
      runSim [d1, d2] `shouldBe` Just 3
