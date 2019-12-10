module Data.Types exposing (..)

import Time

type alias InitFlags = 
    { currentTime: Int }


type PageData
    = ChoreListingPage
    | ChoreAttemptPage ChoreAttempt


type alias AppState =
    { currentTime: Time.Posix
    , chores : List Chore
    , attempts : List ChoreAttempt
    , pageData : PageData
    }


type AppMsg
    = CreateChoreAttempt Chore
    | NavigateToAttempt ChoreAttempt
    | NoMsg


type ChoreStatus
    = InProgress
    | Complete


type ChoreTime
    = DurationInSeconds Int
    | PercentageOfTotal Int


type ChoreReward
    = RewardStars Int
    | MoneyReward Float
    | NoReward


type ChoreIncentive
    = CompletionIncentive ChoreReward
    | HalfTimeIncentive ChoreReward
    | QuarterTimeIncentive ChoreReward


type ChoreAction
    = MoveToStep Int
    | CompleteStep ChoreTime
    | SkipStep ChoreTime


type alias ChoreStep =
    { name : String
    , duration : Maybe ChoreTime
    , incentives : List ChoreIncentive
    }


type alias Chore =
    { id : String
    , name : String
    , reward : ChoreReward
    , steps : List ChoreStep
    }


type alias ChoreAttempt =
    { id : String
    , chore : Chore
    , status : ChoreStatus
    , log : List ChoreAction
    , createdAt : Time.Posix
    }


type ChoreStepStatus 
    = CompletedStep
    | SkippedStep
    | CurrentStep


type alias ChoreStepState =
    { choreStep: ChoreStep
    , secondsRemaining: Int
    , status: ChoreStepStatus
    }


type alias ChoreAttemptState = 
    { elapsedTime: Int
    , stepStatus: List ChoreStepState
    }


