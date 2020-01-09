module Data.Types exposing (..)

import Time


type alias InitFlags =
    { currentTime : Int }


type alias ChoreId =
    String


type alias ChoreAttemptId =
    String


type PageData
    = ChoreListingPage
    | ChoreAttemptPage ChoreAttemptId


type alias AppState =
    { currentTime : Time.Posix
    , chores : List Chore
    , attempts : List ChoreAttempt -- should probably be Array
    , receipts : List StarReceipt
    , pageData : PageData
    }


type AppMsg
    = CreateChoreAttempt Chore
    | NewChoreAttempt ChoreAttempt
    | NavigateToAttempt ChoreAttempt
    | NavigateToChoreList
    | AppendChoreAction ChoreAttempt (List ChoreAction)
    | FinalizeAttempt ChoreAttempt
    | TickClock Time.Posix
    | BigWhoopsie String


type ChoreStatus
    = InProgress
    | Complete Time.Posix


type ChoreTime
    = DurationInMillis Int
    | PercentageOfTotal Float


type alias RewardStars =
    Int


type ChoreIncentive
    = CompletionIncentive RewardStars
    | HalfTimeIncentive RewardStars
    | QuarterTimeIncentive RewardStars


type ChoreAction
    = MoveToStep Int
    | CompleteStep
    | SkipStep


type alias ChoreLogEntry =
    ( Time.Posix, ChoreAction )


type alias ChoreStep =
    { name : String
    , duration : Maybe ChoreTime
    , incentives : List ChoreIncentive
    }


type alias Chore =
    { id : ChoreId
    , name : String
    , reward : RewardStars
    , steps : List ChoreStep
    , durationInMillis : Maybe Int
    }


type alias ChoreAttempt =
    { id : ChoreAttemptId
    , chore : Chore
    , status : ChoreStatus
    , log : List ChoreLogEntry
    , createdAt : Time.Posix
    }


type ChoreStepStatus
    = CompletedStep
    | SkippedStep
    | IdleStep


type alias ChoreStepState =
    { choreStep : ChoreStep
    , stepIndex : Int
    , millisRemaining : Maybe Int
    , durationInMillis : Maybe Int
    , status : ChoreStepStatus
    }


type alias ChoreAttemptState =
    { elapsedMillis : Int
    , stepStates : List ChoreStepState
    , currentStepIndex : Int
    }


type ReceiptReason
    = ChoreCompletionReward ChoreAttemptId
    | Payout String


type alias StarReceipt =
    { amount : Int
    , reason : ReceiptReason
    , createdAt : Time.Posix
    }
