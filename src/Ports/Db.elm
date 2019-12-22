port module Ports.Db exposing (..)

import Data.Types as T
import Json.Encode as E
import Json.Decode as D exposing (Decoder(..))
import Time


port pushChoreAttempt : E.Value -> Cmd T.AppMsg
port choreAttemptAdded : (E.Value -> T.AppMsg) -> Sub T.AppMsg


encodeChoreReward : T.ChoreReward -> E.Value
encodeChoreReward reward =
    case reward of 
        RewardStars stars ->
            E.object [("type", E.string "RewardStars"), ("amount", E.int stars)]

        MoneyReward cents ->
            E.object [("type", E.string "MoneyReward"), ("amount", E.int cents)]

        NoReward ->
            E.object [("type", E.string "NoReward")]


encodeIncentive : T.ChoreIncentive -> E.Value
encodeIncentive incentive =
    case incentive of
        CompletionIncentive reward ->
            E.object [("type", E.string "CompletionIncentive"), ("reward", encodeChoreReward reward)]

        HalfTimeIncentive reward ->
            E.object [("type", E.string "HalfTimeIncentive"), ("reward", encodeChoreReward reward)]

        QuarterTimeIncentive reward ->
            E.object [("type", E.string "QuarterTimeIncentive"), ("reward", encodeChoreReward reward)]


encodeChoreTime : T.ChoreTime -> E.Value
encodeChoreTime time =
    case time of 
        DurationInMillis millis ->
            E.object [("type", E.string "DurationInMillis"), ("amount", E.int millis)]

        PercentageOfTotal percentage ->
            E.object [("type", E.string "PercentageOfTotal"), ("amount", E.int percentage)]


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
        [("name", E.string step.name)
        ,("duration", encodedDuration)]
        ,("incentives", E.list encodeIncentive step.incentives)


encodeChore : T.Chore -> E.Value
encodeChore chore =
    E.object 
        [("id", E.string chore.id)
        ,("name", E.string chore.name)
        ,("reward", encodeChoreReward chore.reward)
        ,("steps", E.list encodeChoreStep chore.steps)
        ,("durationInMillis", E.int durationInMillis)
        ]


choreAttemptDecoder : Decoder T.ChoreAttempt
-- TODO

makeAttemptFromChore : Time.Posix -> T.Chore -> Cmd T.AppMsg
makeAttemptFromChore currentTime chore =
    pushChoreAttempt
        E.object
            [("chore", encodeChore chore)
            ,("status", E.string "InProgress")
            ,("log", E.list never [])
            ,("createdAt", E.int (Time.posixToMillis currentTime))]


onChoreAttemptAdded : Sub T.AppMsg
onChoreAttemptAdded =
    choreAttemptAdded
        (\jsValue -> 
            case (D.decodeValue choreAttemptDecoder jsValue) of
                Ok choreAttempt ->
                    -- TODO

                Err error ->
                    -- TODO: push error back up
        )

