module Main exposing (..)

import Browser
import Data.Chore
import Data.Types as T
import Html as H
import Html.Events
import Pages.ChoreAttempt
import Pages.ChoreList
import Time



-- TODO: Generate IDs in JS and send through ports to elm


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


init : T.InitFlags -> ( T.AppState, Cmd T.AppMsg )
init initFlags =
    ( initialState initFlags, Cmd.none )


subscriptions : T.AppState -> Sub T.AppMsg
subscriptions model =
    Time.every 1000 T.TickClock


view : T.AppState -> H.Html T.AppMsg
view model =
    case model.pageData of
        T.ChoreListingPage ->
            Pages.ChoreList.mainView model.chores model.attempts

        T.ChoreAttemptPage attempt ->
            maybeChoreAttemptPage model attempt (Pages.ChoreAttempt.mainView model.currentTime)


maybeChoreAttemptPage : T.AppState -> T.ChoreAttemptId -> (T.ChoreAttempt -> H.Html T.AppMsg) -> H.Html T.AppMsg
maybeChoreAttemptPage model attemptId pageView =
    let
        matchingAttempts =
            List.filter (\attempt -> attempt.id == attemptId) model.attempts

        emptyPage =
            H.div [] [ H.text "Can't find that chore attempt" ]
    in
    Maybe.withDefault emptyPage (List.head matchingAttempts |> Maybe.map pageView)


makeAttemptFromChore : Time.Posix -> T.Chore -> T.ChoreAttempt
makeAttemptFromChore currentTime chore =
    { id = "xxx"
    , chore = chore
    , status = T.InProgress
    , log = []
    , createdAt = currentTime
    }


update : T.AppMsg -> T.AppState -> ( T.AppState, Cmd T.AppMsg )
update msg model =
    case msg of
        T.CreateChoreAttempt chore ->
            ( { model | attempts = makeAttemptFromChore model.currentTime chore :: model.attempts }, Cmd.none )

        T.NavigateToAttempt attempt ->
            ( { model | pageData = T.ChoreAttemptPage attempt.id }, Cmd.none )

        T.TickClock currentTime ->
            ( { model | currentTime = currentTime }, Cmd.none )

        T.AppendChoreAction choreAttempt newActions ->
            ( appendAttemptChoreAction model newActions choreAttempt.id, Cmd.none )


appendAttemptChoreAction : T.AppState -> List T.ChoreAction -> T.ChoreAttemptId -> T.AppState
appendAttemptChoreAction model newActions attemptId =
    let
        timestampedLogEntries =
            List.map (Tuple.pair model.currentTime) newActions
    in
    updateAttemptById
        model
        (\attempt -> { attempt | log = List.append attempt.log timestampedLogEntries })
        attemptId


updateAttemptById : T.AppState -> (T.ChoreAttempt -> T.ChoreAttempt) -> T.ChoreAttemptId -> T.AppState
updateAttemptById model updateFn id =
    { model
        | attempts =
            List.map
                (\attempt ->
                    if attempt.id == id then
                        updateFn attempt

                    else
                        attempt
                )
                model.attempts
    }
