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
    in
    { elapsedSeconds = elapsedMillis // 1000
    , stepStates = List.indexedMap (choreStepToState currentTime choreAttempt) choreAttempt.chore.steps
    }


choreStepToState : Time.Posix -> T.ChoreAttempt -> Int -> T.ChoreStep -> T.ChoreStepState
choreStepToState currentTime choreAttempt stepIndex choreStep =
    let
        stepState =
            List.foldl
                updateStateWithLog
                { status = T.IdleStep
                , lastLog = choreAttempt.createdAt
                , stepIndex = stepIndex
                , secondsRemaining =
                    case choreStep.duration of
                        Just (T.DurationInSeconds seconds) ->
                            Just seconds

                        Just (T.PercentageOfTotal percentage) ->
                            Just (percentageOfTotalDuration choreAttempt.chore percentage)

                        Nothing ->
                            Nothing
                }
                choreAttempt.log
    in
    { choreStep = choreStep
    , stepIndex = stepIndex
    , secondsRemaining = stepState.secondsRemaining
    , status = stepState.status
    }


type alias StepStateReducer =
    { status : T.ChoreStepStatus
    , lastLog : Time.Posix
    , stepIndex : Int
    , secondsRemaining : Maybe Int
    }


updateStateWithLog : T.ChoreLogEntry -> StepStateReducer -> StepStateReducer
updateStateWithLog ( logTime, action ) state =
    { state
        | status =
            case action of
                T.MoveToStep moveToStepNum ->
                    if moveToStepNum == state.stepIndex then
                        T.CurrentStep

                    else
                        state.status

                T.CompleteStep ->
                    T.CompletedStep

                T.SkipStep ->
                    T.SkippedStep
        , lastLog =
            logTime
    }


percentageOfTotalDuration : T.Chore -> Int -> Int
percentageOfTotalDuration chore percentage =
    Maybe.withDefault 0 <| Maybe.map (\dur -> dur // percentage) chore.durationInSeconds


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
      , durationInSeconds = Just (60 * 10)
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
      , durationInSeconds = Just (60 * 10)
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
