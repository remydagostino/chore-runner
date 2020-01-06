module Pages.ChoreAttempt exposing (..)

import Data.Chore
import Data.Types as T
import Html as H
import Html.Events as HE
import Time


mainView : Time.Posix -> T.ChoreAttempt -> H.Html T.AppMsg
mainView currentTime attempt =
    let
        attemptState =
            Data.Chore.currentAttemptState currentTime attempt
    in
    H.div []
        [ H.h3 [] [ H.text ("In progress: " ++ attempt.chore.name) ]
        , H.h4 [] [ H.text (String.fromInt (attemptState.elapsedMillis // 1000)) ]
        , stepListView attempt attemptState
        , stepDetailView attempt attemptState
        ]


stepListView : T.ChoreAttempt -> T.ChoreAttemptState -> H.Html T.AppMsg
stepListView attempt attemptState =
    H.ol [] <|
        List.map
            (\stepState ->
                let
                    navigateEvent =
                        T.AppendChoreAction attempt [ T.MoveToStep stepState.stepIndex ]
                in
                H.li [ HE.onClick navigateEvent ] [ stepListItemView attemptState stepState ]
            )
            attemptState.stepStates


stepListItemView : T.ChoreAttemptState -> T.ChoreStepState -> H.Html T.AppMsg
stepListItemView attemptState stepState =
    H.div []
        [ H.text stepState.choreStep.name
        , case stepState.status of
            T.CompletedStep ->
                H.text " (complete)"

            T.SkippedStep ->
                H.text " (skipped)"

            _ ->
                H.text ""
        , case stepState.millisRemaining of
            Just millis ->
                H.text (" [" ++ String.fromInt (millis // 1000) ++ "s]")

            Nothing ->
                H.text ""
        , if attemptState.currentStepIndex == stepState.stepIndex then
            H.text " <="

          else
            H.text ""
        ]


stepDetailView : T.ChoreAttempt -> T.ChoreAttemptState -> H.Html T.AppMsg
stepDetailView attempt attemptState =
    let
        incompleteSteps : List T.ChoreStepState
        incompleteSteps =
            List.filter
                (\s -> s.stepIndex > attemptState.currentStepIndex)
                attemptState.stepStates

        nextStepIndex : Maybe Int
        nextStepIndex =
            Maybe.map .stepIndex <| List.head incompleteSteps

        moveToStepActions : List T.ChoreAction
        moveToStepActions =
            Maybe.withDefault [] <| Maybe.map (\i -> [ T.MoveToStep i ]) nextStepIndex

        skipStepEvent =
            T.AppendChoreAction attempt (T.SkipStep :: moveToStepActions)

        doneStepEvent =
            T.AppendChoreAction attempt (T.CompleteStep :: moveToStepActions)
    in
    H.div []
        [ H.button [ HE.onClick skipStepEvent ] [ H.text "Skip" ]
        , H.button [ HE.onClick doneStepEvent ] [ H.text "Done!" ]
        ]
