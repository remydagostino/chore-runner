module Main exposing (..)

import Browser
import Data.Chore
import Data.Types as T
import Html as H
import Html.Events
import Pages.ChoreAttempt
import Pages.ChoreList
import Ports.Db as Db
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
    , receipts = []
    , pageData = T.ChoreListingPage
    }


init : T.InitFlags -> ( T.AppState, Cmd T.AppMsg )
init initFlags =
    ( initialState initFlags, Cmd.none )


subscriptions : T.AppState -> Sub T.AppMsg
subscriptions model =
    Sub.batch
        [ Time.every 1000 T.TickClock
        , Db.onChoreAttemptAdded
        ]


view : T.AppState -> H.Html T.AppMsg
view model =
    case model.pageData of
        T.ChoreListingPage ->
            Pages.ChoreList.mainView model

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


update : T.AppMsg -> T.AppState -> ( T.AppState, Cmd T.AppMsg )
update msg model =
    case msg of
        T.CreateChoreAttempt chore ->
            ( model, Db.makeAttemptFromChore model.currentTime chore )

        T.NewChoreAttempt choreAttempt ->
            ( { model | attempts = choreAttempt :: model.attempts }, Cmd.none )

        T.NavigateToAttempt attempt ->
            ( { model | pageData = T.ChoreAttemptPage attempt.id }, Cmd.none )

        T.NavigateToChoreList ->
            ( { model | pageData = T.ChoreListingPage }, Cmd.none )

        T.AppendChoreAction choreAttempt newActions ->
            if choreAttempt.status == T.InProgress then
                ( appendAttemptChoreAction model newActions choreAttempt.id, Cmd.none )

            else
                ( model, Cmd.none )

        T.FinalizeAttempt attempt ->
            ( finishChoreAttempt model attempt.id, Cmd.none )

        T.TickClock currentTime ->
            ( { model | currentTime = currentTime }, Cmd.none )

        T.BigWhoopsie err ->
            ( model, Cmd.none )


finishChoreAttempt : T.AppState -> T.ChoreAttemptId -> T.AppState
finishChoreAttempt model attemptId =
    let
        model2 =
            updateAttemptById
                model
                (\attempt -> { attempt | status = T.Complete model.currentTime })
                attemptId

        receipt =
            getAttemptById model2 attemptId
                |> Maybe.andThen (Data.Chore.calculateRewardStars model2.currentTime)
    in
    { model2
        | pageData = T.ChoreListingPage
        , receipts = appendMaybe model2.receipts receipt
    }


appendMaybe : List a -> Maybe a -> List a
appendMaybe list maybe =
    case maybe of
        Just a ->
            a :: list

        Nothing ->
            list


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


getAttemptById : T.AppState -> T.ChoreAttemptId -> Maybe T.ChoreAttempt
getAttemptById model attemptId =
    List.head <| List.filter (\{ id } -> attemptId == id) model.attempts


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
