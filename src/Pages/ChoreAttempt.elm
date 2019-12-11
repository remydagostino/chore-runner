module Pages.ChoreAttempt exposing (..)

import Data.Chore
import Data.Types as T
import Html
import Html.Events
import Time


mainView : Time.Posix -> T.ChoreAttempt -> Html.Html T.AppMsg
mainView currentTime attempt =
    let
        attemptState =
            Data.Chore.currentAttemptState currentTime attempt
    in
    Html.div []
        [ Html.h3 [] [ Html.text ("In progress: " ++ attempt.chore.name) ]
        , Html.h4 [] [ Html.text (String.fromInt attemptState.elapsedSeconds) ]
        , stepListView attemptState.stepStates
        ]


stepListView : List T.ChoreStepState -> Html.Html T.AppMsg
stepListView stepStates =
    Html.ol [] (List.map (\state -> Html.li [] [ stepListItemView state ]) stepStates)


stepListItemView : T.ChoreStepState -> Html.Html T.AppMsg
stepListItemView state =
    Html.div []
        [ Html.text state.choreStep.name
        , case state.status of
            T.CurrentStep ->
                Html.text " [current]"

            _ ->
                Html.text ""
        ]
