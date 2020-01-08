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
    let
        matchingAttempts =
            List.filter (\attempt -> attempt.chore == chore) attempts
    in
    Html.li
        []
        [ Html.div [ Html.Events.onClick (T.CreateChoreAttempt chore) ] [ Html.text chore.name ]
        , Html.ul [] (List.map choreAttemptView matchingAttempts)
        ]


choreAttemptView : T.ChoreAttempt -> Html.Html T.AppMsg
choreAttemptView attempt =
    Html.li
        [ Html.Events.onClick (T.NavigateToAttempt attempt) ]
        [ case attempt.status of
            T.InProgress ->
                Html.text "In progress"

            T.Complete time ->
                Html.text "Complete"
        ]
