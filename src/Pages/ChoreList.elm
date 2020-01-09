module Pages.ChoreList exposing (..)

import Data.Chore
import Data.Types as T
import Html
import Html.Events
import Time


mainView : T.AppState -> Html.Html T.AppMsg
mainView { chores, attempts, receipts } =
    Html.div []
        [ starCountView receipts
        , Html.ol [] <|
            List.map (\chore -> choreView chore attempts) chores
        ]


starCountView : List T.StarReceipt -> Html.Html T.AppMsg
starCountView receipts =
    let
        totalStars =
            List.sum (List.map .amount receipts)
    in
    Html.h2
        []
        [ Html.text ("Stars: " ++ String.fromInt totalStars) ]


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
