module Data.Chore exposing (..)

import Data.Types as T exposing (Chore, ChoreAttempt, ChoreAttemptState)
import Time exposing (posixToMillis)
import Array as Arr exposing (Array)

type alias FoldedLog =
    { millisSpentPerStep : Array Int
    , statusPerStep : Array T.ChoreStepStatus
    , currentStepIndex : Int
    , lastUpdated : Time.Posix
    }

aGetWithDefault : a -> Int -> Array a -> a
aGetWithDefault default index arr =
    Maybe.withDefault default (Arr.get index arr)

alterArrayItem : Int -> (Maybe a -> a) -> Array a -> Array a
alterArrayItem index update arr = 
    Arr.set 
        index
        (update (Arr.get index arr))
        arr


millisBetween : Time.Posix -> Time.Posix -> Int
millisBetween from to =
    (posixToMillis to) - (posixToMillis from)


currentAttemptState : Time.Posix -> ChoreAttempt -> ChoreAttemptState
currentAttemptState currentTime choreAttempt =
    let
        foldedLog =
            foldForwardLog currentTime choreAttempt
    in
    { elapsedMillis = millisBetween choreAttempt.createdAt currentTime
    , stepStates = List.indexedMap (choreStepToState choreAttempt foldedLog) choreAttempt.chore.steps
    , currentStepIndex = foldedLog.currentStepIndex
    }


choreStepToState : T.ChoreAttempt -> FoldedLog -> Int -> T.ChoreStep -> T.ChoreStepState
choreStepToState choreAttempt foldedLog stepIndex choreStep =
    let
        durationInMillis =
            case choreStep.duration of
                Just (T.DurationInMillis millis) ->
                    Just millis

                Just (T.PercentageOfTotal percentage) ->
                    Just (percentageOfTotalDuration choreAttempt.chore percentage)

                Nothing ->
                    Nothing
    in
    { choreStep = 
        choreStep

    , stepIndex = 
        stepIndex

    , millisRemaining = 
        Maybe.map
            (\millis -> max 0 (millis - (aGetWithDefault 0 stepIndex foldedLog.millisSpentPerStep)))
            durationInMillis 

    , status = 
        aGetWithDefault T.IdleStep stepIndex foldedLog.statusPerStep
    }

foldForwardLog : Time.Posix -> T.ChoreAttempt -> FoldedLog
foldForwardLog currentTime choreAttempt =
    let
        numberOfSteps = 
            List.length choreAttempt.chore.steps

        foldedLog =
            List.foldl
                updateStateWithLog
                { millisSpentPerStep = Arr.repeat numberOfSteps 0
                , statusPerStep = Arr.repeat numberOfSteps T.IdleStep
                , currentStepIndex = 0
                , lastUpdated = choreAttempt.createdAt
                }
                choreAttempt.log

        millisUntilNow = 
            millisBetween foldedLog.lastUpdated currentTime

        updatedWithCurrentTime = 
            { foldedLog 
                | lastUpdated = currentTime 
                , millisSpentPerStep =
                    alterArrayItem
                        foldedLog.currentStepIndex
                        (\millis -> (Maybe.withDefault 0 millis) + millisUntilNow)
                        foldedLog.millisSpentPerStep

            }
    in
    updatedWithCurrentTime



updateStateWithLog : T.ChoreLogEntry -> FoldedLog -> FoldedLog
updateStateWithLog ( logTime, action ) state =
    let
        millisSpentInCurrentStep = 
            millisBetween state.lastUpdated logTime

        changeToCurrentStatus = 
            case action of
                T.MoveToStep moveToStepNum ->
                    Nothing

                T.CompleteStep ->
                    Just T.CompletedStep

                T.SkipStep ->
                    Just T.SkippedStep

        changeToCurrentStepIndex = 
            case action of
                T.MoveToStep moveToStepNum ->
                    Just moveToStepNum

                _ ->
                    Nothing
    in
    { millisSpentPerStep = 
        alterArrayItem
            state.currentStepIndex
            (\millis -> (Maybe.withDefault 0 millis) + millisSpentInCurrentStep)
            state.millisSpentPerStep

    , statusPerStep = 
        case changeToCurrentStatus of
            Just newStatus ->
                Arr.set
                    state.currentStepIndex 
                    newStatus
                    state.statusPerStep

            Nothing -> 
                state.statusPerStep

    , currentStepIndex = 
        Maybe.withDefault state.currentStepIndex changeToCurrentStepIndex

    , lastUpdated = 
        logTime
    }


percentageOfTotalDuration : T.Chore -> Int -> Int
percentageOfTotalDuration chore percentage =
    Maybe.withDefault 0 <| Maybe.map (\dur -> dur // percentage) chore.durationInMillis


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
      , durationInMillis = Just (60 * 1000 * 10)
      , steps =
            [ { name = "Make bed"
              , duration = Just (T.DurationInMillis (60 * 1000 * 2))
              , incentives = standardSpeedIncentives
              }
            , { name = "Put on clothes"
              , duration = Just (T.DurationInMillis (60 * 1000 * 5))
              , incentives = standardSpeedIncentives
              }
            , { name = "Feed Myska"
              , duration = Just (T.DurationInMillis (60 * 1000 * 4))
              , incentives = standardSpeedIncentives
              }
            ]
      }
    , { id = "2"
      , name = "Get ready for bed"
      , reward = T.RewardStars 1
      , durationInMillis = Just (60 * 1000 * 10)
      , steps =
            [ { name = "Clean up mess"
              , duration = Just (T.DurationInMillis (60 * 1000 * 10))
              , incentives = standardSpeedIncentives
              }
            , { name = "Pack bag"
              , duration = Just (T.DurationInMillis (60 * 1000 * 8))
              , incentives = standardSpeedIncentives
              }
            , { name = "Brush teeth"
              , duration = Just (T.DurationInMillis (60 * 1000 * 6))
              , incentives = standardSpeedIncentives
              }
            ]
      }
    ]
