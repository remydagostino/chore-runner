module Main exposing (..)

import Browser
import Data.Types as T
import Data.Chore
import Pages.ChoreAttempt
import Pages.ChoreList
import Html
import Html.Events
import Time


main =
    Browser.element 
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view 
        }


initialState : T.InitFlags -> T.AppState
initialState initFlags =
    { currentTime = Time.millisToPosix initFlags.currentTime
    , chores = Data.Chore.basicChores
    , attempts = []
    , pageData = T.ChoreListingPage
    }


init : T.InitFlags -> (T.AppState, Cmd T.AppMsg)
init initFlags = 
    (initialState initFlags, Cmd.none)


subscriptions : T.AppState -> Sub T.AppMsg
subscriptions model = 
    Sub.none

view : T.AppState -> Html.Html T.AppMsg
view model =
    case model.pageData of
        T.ChoreListingPage ->
            Pages.ChoreList.mainView model.chores model.attempts
        T.ChoreAttemptPage attempt ->
            Pages.ChoreAttempt.mainView attempt


makeAttemptFromChore : Time.Posix -> T.Chore -> T.ChoreAttempt
makeAttemptFromChore currentTime chore =
    { id = "xxx"
    , chore = chore
    , status = T.InProgress
    , log = []
    , createdAt = currentTime
    }


update : T.AppMsg -> T.AppState -> (T.AppState, Cmd T.AppMsg)
update msg model =
    case msg of
        T.CreateChoreAttempt chore ->
            ({ model | attempts = (makeAttemptFromChore model.currentTime chore) :: model.attempts }, Cmd.none)
        T.NavigateToAttempt attempt ->
            ({ model | pageData = T.ChoreAttemptPage attempt }, Cmd.none)
        T.NoMsg -> (model, Cmd.none)
