module Data.Chore exposing (..)

import Data.Types as T exposing (Chore, ChoreAttempt, ChoreAttemptState)

currentAttemptStatus : ChoreAttempt -> ChoreAttemptState
currentAttemptStatus choreAttempt =
    { elapsedTime = 0
    , stepStatus = []
    } 


standardSpeedIncentives =
    [ T.CompletionIncentive (T.RewardStars 1)
    , T.HalfTimeIncentive (T.RewardStars 1)
    , T.QuarterTimeIncentive (T.RewardStars 1)
    ]


basicChores : List Chore
basicChores =
    [ { id = "1"
      , name = "Get ready for school"
      , reward = T.NoReward
      , steps =
            [ { name = "Make bed"
              , duration = Just (T.DurationInSeconds (60 * 2))
              , incentives = standardSpeedIncentives
              }
            , { name = "Put on clothes"
              , duration = Just (T.DurationInSeconds (60 * 5))
              , incentives = standardSpeedIncentives
              }
            , { name = "Feed Myska"
              , duration = Just (T.DurationInSeconds (60 * 4))
              , incentives = standardSpeedIncentives
              }
            ]
      }
    , { id = "2"
      , name = "Get ready for bed"
      , reward = T.RewardStars 1
      , steps =
            [ { name = "Clean up mess"
              , duration = Just (T.DurationInSeconds (60 * 10))
              , incentives = standardSpeedIncentives
              }
            , { name = "Pack bag"
              , duration = Just (T.DurationInSeconds (60 * 8))
              , incentives = standardSpeedIncentives
              }
            , { name = "Brush teeth"
              , duration = Just (T.DurationInSeconds (60 * 6))
              , incentives = standardSpeedIncentives
              }
            ]
      }
    ]


