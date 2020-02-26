module Page.Start exposing (Model, Msg, init, subscriptions, update, view)

import Data.StartEnd as StartEnd
import Env exposing (Env)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as E
import Ports
import Task
import Time



-- MODEL


type alias Model =
    { state : State
    , zone : Time.Zone
    , time : Time.Posix
    , startEnd : StartEnd.Date
    }


type State
    = Init
    | Waiting
    | Running
    | Finished
    | Error


init : StartEnd.Date -> ( Model, Cmd Msg )
init startEnd =
    ( Model Init Time.utc (Time.millisToPosix 0) startEnd
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = AdjustTimeZone Time.Zone
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )

        Tick time ->
            let
                s =
                    timeToState (StartEnd.posixToPoint model.zone model.time) model.startEnd

                c =
                    case model.state of
                        Waiting ->
                            case s of
                                Running ->
                                    Ports.playSound (E.string "sound-start")

                                _ ->
                                    Cmd.none

                        Running ->
                            case s of
                                Finished ->
                                    Ports.playSound (E.string "sound-finish")

                                _ ->
                                    Cmd.none

                        _ ->
                            Cmd.none
            in
            ( { model | time = time, state = s }, c )



-- VIEW


view : Model -> Html msg
view model =
    let
        now =
            StartEnd.posixToPoint model.zone model.time
    in
    div []
        [ div []
            [ audio
                [ id "sound-start"
                , src (Env.appTop ++ "sound/start.mp3")
                , controls False
                ]
                []
            , audio
                [ id "sound-finish"
                , src (Env.appTop ++ "sound/finish.mp3")
                , controls False
                ]
                []
            ]
        , br [] []
        , div [ class "date-list" ]
            [ div [ class "date-list__body" ]
                [ viewDatePoint now "now"
                , viewDatePoint model.startEnd.start "start"
                , viewDatePoint model.startEnd.end "end"
                ]
            ]
        , case model.state of
            Init ->
                div [] [ text "Init" ]

            Waiting ->
                div [ class "state-waiting" ] [ text "Waiting" ]

            Running ->
                div [ class "state-running" ] [ text "Running" ]

            Finished ->
                div [ class "state-finished" ] [ text "Finished" ]

            Error ->
                div [ class "state-error" ] [ text "Error" ]
        ]


timeToState : StartEnd.Point -> StartEnd.Date -> State
timeToState now startEnd =
    let
        dcn =
            StartEnd.toCompare now

        dcs =
            StartEnd.toCompare startEnd.start

        dce =
            StartEnd.toCompare startEnd.end
    in
    if dcs > dce then
        Error

    else if dcn < dcs then
        Waiting

    else if (dcs <= dcn) && (dcn < dce) then
        Running

    else if dce <= dcn then
        Finished

    else
        Error


viewDatePoint : StartEnd.Point -> String -> Html msg
viewDatePoint d topic =
    let
        sd =
            StartEnd.toStringPoint d
    in
    h2 [] [ text (sd.day ++ "th_" ++ sd.hour ++ ":" ++ sd.minute ++ ":" ++ sd.second ++ "◀" ++ topic) ]



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Time.every 100 Tick
