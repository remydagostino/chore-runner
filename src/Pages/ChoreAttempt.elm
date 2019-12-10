module Pages.ChoreAttempt exposing (..)

import Data.Types as T
import Data.Chore
import Html
import Html.Events
import Time


mainView : T.ChoreAttempt -> Html.Html T.AppMsg 
mainView attempt =
    Html.div [] 
        [ Html.h3 [] [Html.text ("In progress: " ++ attempt.chore.name)]
        ]