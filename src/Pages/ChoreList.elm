module Pages.ChoreList exposing (..)

import Data.Chore
import Data.Types as T
import Html
import Html.Events
import Time


mainView : List T.Chore -> List T.ChoreAttempt -> Html.Html T.AppMsg
mainView chores attempts =
    Html.div []
        [ Html.ol [] <|
            List.map (\chore -> choreView chore attempts) chores
        ]


choreView : T.Chore -> List T.ChoreAttempt -> Html.Html T.AppMsg
choreView chore attempts =
    case List.filter (\attempt -> attempt.chore == chore) attempts of
        [] ->
            Html.li
                [ Html.Events.onClick (T.CreateChoreAttempt chore) ]
                [ Html.text chore.name ]

        matchingAttempts ->
            Html.li []
                [ Html.text chore.name
                , Html.ul [] (List.map choreAttemptView matchingAttempts)
                ]


choreAttemptView : T.ChoreAttempt -> Html.Html T.AppMsg
choreAttemptView attempt =
    Html.li
        [ Html.Events.onClick (T.NavigateToAttempt attempt) ]
        [ Html.text "In progress" ]
