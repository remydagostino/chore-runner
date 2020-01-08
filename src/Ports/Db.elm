port module Ports.Db exposing (..)

import Data.Types as T
import Json.Decode as D exposing (Decoder(..))
import Json.Encode as E
import Time


port pushChoreAttempt : E.Value -> Cmd msg


port choreAttemptAdded : (E.Value -> msg) -> Sub msg


posixToMillisDecoder : Decoder Time.Posix
posixToMillisDecoder =
    D.map Time.millisToPosix D.int


encodeChoreReward : T.ChoreReward -> E.Value
encodeChoreReward reward =
    case reward of
        T.RewardStars stars ->
            E.object [ ( "type", E.string "RewardStars" ), ( "amount", E.int stars ) ]

        T.MoneyReward cents ->
            E.object [ ( "type", E.string "MoneyReward" ), ( "amount", E.int cents ) ]

        T.NoReward ->
            E.object [ ( "type", E.string "NoReward" ) ]


choreRewardDecoder : Decoder T.ChoreReward
choreRewardDecoder =
    D.field "type" D.string
        |> D.andThen
            (\choreRewardType ->
                case choreRewardType of
                    "RewardStars" ->
                        D.map T.RewardStars (D.field "amount" D.int)

                    "MoneyReward" ->
                        D.map T.MoneyReward (D.field "amount" D.int)

                    "NoReward" ->
                        D.succeed T.NoReward

                    _ ->
                        D.fail "Unknown chore reward type"
            )


encodeIncentive : T.ChoreIncentive -> E.Value
encodeIncentive incentive =
    case incentive of
        T.CompletionIncentive reward ->
            E.object [ ( "type", E.string "CompletionIncentive" ), ( "reward", encodeChoreReward reward ) ]

        T.HalfTimeIncentive reward ->
            E.object [ ( "type", E.string "HalfTimeIncentive" ), ( "reward", encodeChoreReward reward ) ]

        T.QuarterTimeIncentive reward ->
            E.object [ ( "type", E.string "QuarterTimeIncentive" ), ( "reward", encodeChoreReward reward ) ]


incentiveDecoder : Decoder T.ChoreIncentive
incentiveDecoder =
    D.field "type" D.string
        |> D.andThen
            (\incentiveType ->
                case incentiveType of
                    "CompletionIncentive" ->
                        D.map T.CompletionIncentive (D.field "reward" choreRewardDecoder)

                    "HalfTimeIncentive" ->
                        D.map T.HalfTimeIncentive (D.field "reward" choreRewardDecoder)

                    "QuarterTimeIncentive" ->
                        D.map T.QuarterTimeIncentive (D.field "reward" choreRewardDecoder)

                    _ ->
                        D.fail "Unknown chore incentive type"
            )


encodeChoreTime : T.ChoreTime -> E.Value
encodeChoreTime time =
    case time of
        T.DurationInMillis millis ->
            E.object [ ( "type", E.string "DurationInMillis" ), ( "amount", E.int millis ) ]

        T.PercentageOfTotal percentage ->
            E.object [ ( "type", E.string "PercentageOfTotal" ), ( "amount", E.int percentage ) ]


choreTimeDecoder : Decoder T.ChoreTime
choreTimeDecoder =
    D.field "type" D.string
        |> D.andThen
            (\timeType ->
                case timeType of
                    "DurationInMillis" ->
                        D.map T.DurationInMillis (D.field "amount" D.int)

                    "PercentageOfTotal" ->
                        D.map T.PercentageOfTotal (D.field "amount" D.int)

                    _ ->
                        D.fail "Unknown chore time type"
            )


encodeChoreStep : T.ChoreStep -> E.Value
encodeChoreStep step =
    let
        encodedDuration =
            case step.duration of
                Just duration ->
                    encodeChoreTime duration

                Nothing ->
                    E.null
    in
    E.object
        [ ( "name", E.string step.name )
        , ( "duration", encodedDuration )
        , ( "incentives", E.list encodeIncentive step.incentives )
        ]


choreStepDecoder : Decoder T.ChoreStep
choreStepDecoder =
    D.map3 T.ChoreStep
        (D.field "name" D.string)
        (D.field "duration" (D.maybe choreTimeDecoder))
        (D.field "incentives" (D.list incentiveDecoder))


encodeChore : T.Chore -> E.Value
encodeChore chore =
    E.object
        [ ( "id", E.string chore.id )
        , ( "name", E.string chore.name )
        , ( "reward", encodeChoreReward chore.reward )
        , ( "steps", E.list encodeChoreStep chore.steps )
        , ( "durationInMillis", Maybe.withDefault E.null (Maybe.map E.int chore.durationInMillis) )
        ]


choreDecoder : Decoder T.Chore
choreDecoder =
    D.map5 T.Chore
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "reward" choreRewardDecoder)
        (D.field "steps" (D.list choreStepDecoder))
        (D.field "durationInMillis" (D.maybe D.int))


encodeChoreStatus : T.ChoreStatus -> E.Value
encodeChoreStatus status =
    case status of
        T.InProgress ->
            E.object [ ( "type", E.string "InProgress" ) ]

        T.Complete completionTime ->
            E.object [ ( "type", E.string "Complete" ), ( "time", E.int (Time.posixToMillis completionTime) ) ]


choreStatusDecoder : Decoder T.ChoreStatus
choreStatusDecoder =
    D.field "type" D.string
        |> D.andThen
            (\statusType ->
                case statusType of
                    "InProgress" ->
                        D.succeed T.InProgress

                    "Complete" ->
                        D.map T.Complete (D.field "time" posixToMillisDecoder)

                    _ ->
                        D.fail "Unknown chore attempt status"
            )


choreActionDecoder : Decoder T.ChoreAction
choreActionDecoder =
    D.field "type" D.string
        |> D.andThen
            (\choreActionType ->
                case choreActionType of
                    "MoveToStep" ->
                        D.map T.MoveToStep (D.field "step" D.int)

                    "CompleteStep" ->
                        D.succeed T.CompleteStep

                    "SkipStep" ->
                        D.succeed T.SkipStep

                    _ ->
                        D.fail "Unknown chore action type"
            )


choreLogDecoder : Decoder T.ChoreLogEntry
choreLogDecoder =
    D.map2 (\time action -> ( time, action ))
        (D.index 0 posixToMillisDecoder)
        (D.index 1 choreActionDecoder)


choreAttemptDecoder : Decoder T.ChoreAttempt
choreAttemptDecoder =
    D.map5 T.ChoreAttempt
        (D.field "id" D.string)
        (D.field "chore" choreDecoder)
        (D.field "status" choreStatusDecoder)
        (D.field "log" (D.list choreLogDecoder))
        (D.field "createdAt" posixToMillisDecoder)


makeAttemptFromChore : Time.Posix -> T.Chore -> Cmd T.AppMsg
makeAttemptFromChore currentTime chore =
    pushChoreAttempt <|
        E.object
            [ ( "chore", encodeChore chore )
            , ( "status", encodeChoreStatus T.InProgress )
            , ( "log", E.list never [] )
            , ( "createdAt", E.int (Time.posixToMillis currentTime) )
            ]


onChoreAttemptAdded : Sub T.AppMsg
onChoreAttemptAdded =
    choreAttemptAdded
        (\jsValue ->
            case D.decodeValue choreAttemptDecoder jsValue of
                Ok choreAttempt ->
                    T.NewChoreAttempt choreAttempt

                Err error ->
                    T.BigWhoopsie (D.errorToString error)
        )
