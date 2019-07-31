module Lib where

{- Bus Gossip

Bus drivers like to share gossip on their route. Find out how many
stops it takes for all drivers to collect all of the gossip from their
peers.

- A route consists of at least 1 stop
- It takes a driver 1 minute to travel between stops
- A driver's route repeats indefinitely all day
- Drivers may have routes of differing lengths
- A day consists of 480 minutes
- At each stop the driver will share their gossip with all other drivers
  at that stop simultaneously and instantly

Examples:

  Route 1: [1, 2, 3, 1]

  Will cycle: 1, 2, 3, 1, 1, 2, 3, 1, 1, 2, 1 ...

  Note that even when '1' appears twice, the driver travels one minute
  back to the same stop.

-}

import Control.Monad.State
import Data.List.NonEmpty (NonEmpty())
import qualified Data.List.NonEmpty as NE

newtype Route = Route (NonEmpty Int)
  deriving (Eq, Show)

type Stop = Int

-- | Returns the next stop in the Route
nextStop :: Stop -> Route -> Stop
nextStop current (Route route) = (current + 1) `mod` NE.length route

type DriverId = Char

data Driver
  = Driver
  { driverId      :: DriverId
  , driverRoute   :: Route
  , driverStop    :: Stop
  , driverGossips :: [DriverId]
  }
  deriving (Eq, Show)

-- | Moves the driver to the next stop on their route
driveToNextStop :: Driver -> Driver
driveToNextStop driver = driver { driverStop = nextStop currentStop route }
  where
    currentStop = driverStop driver
    route       = driverRoute driver

data Sim
  = Sim
  { simDrivers :: [Driver]
  , simMinute  :: Int
  }
  deriving (Eq, Show)

moveDrivers :: State Sim ()
moveDrivers = do
  state <- get
  let state' = state { simDrivers = map driveToNextStop $ simDrivers state }
  put state'

shareGossip :: [Driver] -> [Driver]
shareGossip drivers = map getGossips drivers
  where
    getGossips driver
      = driver
      { driverGossips =
        [ driverId d | d <- drivers
                     , driverId d /= driverId driver
                     , stopId d == stopId driver ] }
    stopId (Driver _ (Route r) s _) = r NE.!! s

updateDriverGossips :: State Sim ()
updateDriverGossips = do
  state <- get
  let state' = state { simDrivers = shareGossip $ simDrivers state }
  put state'

tickSimTime :: State Sim ()
tickSimTime = do
  state <- get
  let state' = state { simMinute = simMinute state + 1 }
  put state'

updateSim :: State Sim (Maybe Int)
updateSim = do
  moveDrivers
  updateDriverGossips
  tickSimTime
  sim <- get
  if allGossipShared (simDrivers sim)
    then return $ Just (simMinute sim)
    else if (simMinute sim) > 480
         then return Nothing
         else updateSim

allGossipShared :: [Driver] -> Bool
allGossipShared [] = False -- nobody on the routes to share
allGossipShared [_] = False -- can't share with yourself
allGossipShared drivers = and . map hasReceivedAllGossip $ drivers
  where
    hasReceivedAllGossip driver =
      length (driverGossips driver) == length drivers - 1

runSim :: [Driver] -> Maybe Int
runSim drivers = fst $ runState updateSim (Sim drivers 0)
