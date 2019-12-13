module Data.Chore exposing (..)

import Data.Types as T exposing (Chore, ChoreAttempt, ChoreAttemptState)
import Time


currentAttemptState : Time.Posix -> ChoreAttempt -> ChoreAttemptState
currentAttemptState currentTime choreAttempt =
    let
        currentTimeMillis =
            Time.posixToMillis currentTime

        elapsedMillis =
            currentTimeMillis - Time.posixToMillis choreAttempt.createdAt

        currentStepIndex =
            List.foldl
                (\action lastStepNum ->
                    case action of
                        T.MoveToStep moveNum ->
                            moveNum

                        _ ->
                            lastStepNum
                )
                0
                (List.map Tuple.second choreAttempt.log)
    in
    { elapsedSeconds = elapsedMillis // 1000
    , stepStates =
        List.indexedMap
            (\index choreStep ->
                { choreStep = choreStep
                , stepIndex = index
                , secondsRemaining = 1
                , status =
                    if index == currentStepIndex then
                        T.CurrentStep

                    else
                        T.IdleStep
                }
            )
            choreAttempt.chore.steps
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
