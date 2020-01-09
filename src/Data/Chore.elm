module Data.Chore exposing (..)

import Array as Arr exposing (Array)
import Data.Types as T exposing (Chore, ChoreAttempt, ChoreAttemptState)
import Time exposing (posixToMillis)


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
    posixToMillis to - posixToMillis from


currentAttemptState : Time.Posix -> ChoreAttempt -> ChoreAttemptState
currentAttemptState currentTime choreAttempt =
    let
        endTime =
            case choreAttempt.status of
                T.InProgress ->
                    currentTime

                T.Complete completionTime ->
                    completionTime

        foldedLog =
            foldForwardLog endTime choreAttempt
    in
    { elapsedMillis = millisBetween choreAttempt.createdAt endTime
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
            (\millis -> max 0 (millis - aGetWithDefault 0 stepIndex foldedLog.millisSpentPerStep))
            durationInMillis
    , durationInMillis =
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
                        (\millis -> Maybe.withDefault 0 millis + millisUntilNow)
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
            (\millis -> Maybe.withDefault 0 millis + millisSpentInCurrentStep)
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


percentageOfTotalDuration : T.Chore -> Float -> Int
percentageOfTotalDuration chore percentage =
    Maybe.withDefault 0 <| Maybe.map (\dur -> round (toFloat dur * percentage)) chore.durationInMillis


sumIncentives : T.ChoreStepState -> Int
sumIncentives stepState =
    let
        timeFactor =
            Maybe.withDefault 1 <|
                Maybe.map2
                    (\total remaining -> toFloat remaining / toFloat total)
                    stepState.durationInMillis
                    stepState.millisRemaining

        incentiveStars =
            List.map
                (\incentive ->
                    case incentive of
                        T.CompletionIncentive stars ->
                            stars

                        T.HalfTimeIncentive stars ->
                            if timeFactor >= 0.5 then
                                stars

                            else
                                0

                        T.QuarterTimeIncentive stars ->
                            if timeFactor >= 0.75 then
                                stars

                            else
                                0
                )
                stepState.choreStep.incentives
    in
    case stepState.status of
        T.CompletedStep ->
            List.sum incentiveStars

        _ ->
            0


calculateRewardStars : Time.Posix -> T.ChoreAttempt -> Maybe T.StarReceipt
calculateRewardStars currentTime attempt =
    let
        attemptState =
            currentAttemptState currentTime attempt

        stepStars =
            List.map sumIncentives attemptState.stepStates
    in
    case attempt.status of
        T.Complete _ ->
            Just
                { amount = List.sum stepStars + attempt.chore.reward
                , reason = T.ChoreCompletionReward attempt.id
                , createdAt = currentTime
                }

        T.InProgress ->
            Nothing


standardSpeedIncentives =
    [ T.CompletionIncentive 1
    , T.HalfTimeIncentive 1
    , T.QuarterTimeIncentive 1
    ]


basicChores : List Chore
basicChores =
    [ { id = "1"
      , name = "Get ready for school"
      , reward = 0
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
      , reward = 1
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
    , { id = "3"
      , name = "Make a cup of tea"
      , reward = 1
      , durationInMillis = Just (60 * 1000 * 4)
      , steps =
            [ { name = "Fill kettle"
              , duration = Just (T.PercentageOfTotal 0.2)
              , incentives = standardSpeedIncentives
              }
            , { name = "Boil kettle"
              , duration = Nothing
              , incentives = [ T.CompletionIncentive 1 ]
              }
            , { name = "Steep tea"
              , duration = Just (T.PercentageOfTotal 0.4)
              , incentives = [ T.CompletionIncentive 1 ]
              }
            , { name = "Remove tea bag"
              , duration = Just (T.PercentageOfTotal 0.2)
              , incentives = standardSpeedIncentives
              }
            , { name = "Add milk"
              , duration = Just (T.PercentageOfTotal 0.2)
              , incentives = standardSpeedIncentives
              }
            ]
      }
    , { id = "4"
      , name = "Empty bins"
      , reward = 0
      , durationInMillis = Just (60 * 1000 * 1)
      , steps =
            [ { name = "Restmull"
              , duration = Just (T.PercentageOfTotal 0.2)
              , incentives = standardSpeedIncentives
              }
            , { name = "Werkstoffe"
              , duration = Just (T.PercentageOfTotal 0.2)
              , incentives = standardSpeedIncentives
              }
            , { name = "Paper & Cardboard"
              , duration = Just (T.PercentageOfTotal 0.2)
              , incentives = standardSpeedIncentives
              }
            ]
      }
    ]
