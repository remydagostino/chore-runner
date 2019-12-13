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
        , H.h4 [] [ H.text (String.fromInt attemptState.elapsedSeconds) ]
        , stepListView attemptState.stepStates
        , stepDetailView attempt attemptState
        ]


stepListView : List T.ChoreStepState -> H.Html T.AppMsg
stepListView stepStates =
    H.ol [] (List.map (\state -> H.li [] [ stepListItemView state ]) stepStates)


stepListItemView : T.ChoreStepState -> H.Html T.AppMsg
stepListItemView state =
    H.div []
        [ H.text state.choreStep.name
        , case state.status of
            T.CurrentStep ->
                H.text " [current]"

            _ ->
                H.text ""
        ]


stepDetailView : T.ChoreAttempt -> T.ChoreAttemptState -> H.Html T.AppMsg
stepDetailView attempt attemptState =
    let
        firstCurrentStep : Maybe T.ChoreStepState
        firstCurrentStep =
            List.head <|
                List.filter
                    (\stepState -> stepState.status == T.CurrentStep)
                    attemptState.stepStates

        currentStepIndex : Int
        currentStepIndex =
            Maybe.withDefault 0 <| Maybe.map .stepIndex firstCurrentStep

        incompleteSteps : List T.ChoreStepState
        incompleteSteps =
            List.filter
                (\s -> s.status /= T.CurrentStep && s.stepIndex > currentStepIndex)
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
