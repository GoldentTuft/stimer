module Page.Top exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Navigation
import Data.StartEnd as StartEnd
import Env exposing (Env)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as E
import Looper as Lp
import Ports
import Task
import Time
import Url



-- MODEL


type alias Model =
    { state : State
    , url : Url.Url
    , zone : Time.Zone
    , now : Time.Posix
    , startEnd : StartEnd.Date
    }


type State
    = Init


init : Url.Url -> ( Model, Cmd Msg )
init url =
    ( Model Init
        url
        Time.utc
        (Time.millisToPosix 0)
        StartEnd.newDate
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = AdjustTimeZone Time.Zone
    | AdjustTime Time.Posix
    | UpdateDays Int PointType
    | UpdateHours Int PointType
    | UpdateMinutes Int PointType
    | Tick Time.Posix
    | OpenLink
    | CopyLink



--| LoadLink


type PointType
    = Start
    | End


updateDays : Int -> StartEnd.Point -> StartEnd.Point
updateDays amount point =
    { point | day = Lp.new point.day 1 31 |> Lp.add amount |> Lp.get }


updateHours : Int -> StartEnd.Point -> StartEnd.Point
updateHours amount point =
    { point | hour = Lp.new point.hour 0 23 |> Lp.add amount |> Lp.get }


updateMinutes : Int -> StartEnd.Point -> StartEnd.Point
updateMinutes amount point =
    { point | minute = Lp.new point.minute 0 59 |> Lp.add amount |> Lp.get }


downMinutes : Int -> StartEnd.Point -> StartEnd.Point
downMinutes step point =
    let
        newMinutes =
            if (point.minute - step) < 0 then
                59

            else
                point.minute - step
    in
    { point | minute = newMinutes }


upMinutes : Int -> StartEnd.Point -> StartEnd.Point
upMinutes step point =
    let
        newMinutes =
            if (point.minute + step) > 59 then
                0

            else
                point.minute + step
    in
    { point | minute = newMinutes }


{-| やめた。思い通りにならない。「3」を消して「4」を入力しようとかできない。諦める。
]
-}
inputDays : String -> StartEnd.Point -> StartEnd.Point
inputDays days point =
    case String.toInt days of
        Nothing ->
            point

        Just num ->
            if (1 <= num) && (num <= 31) then
                { point | day = num }

            else
                point


setStartPoint : Model -> StartEnd.Point -> Model
setStartPoint model point =
    let
        date =
            model.startEnd
    in
    { model | startEnd = { date | start = point } }


setEndPoint : Model -> StartEnd.Point -> Model
setEndPoint model point =
    let
        date =
            model.startEnd
    in
    { model | startEnd = { date | end = point } }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone zone ->
            ( { model | zone = zone }, Task.perform AdjustTime Time.now )

        AdjustTime time ->
            let
                newStart =
                    StartEnd.posixToPoint model.zone time

                newEnd =
                    StartEnd.posixToPoint model.zone time

                newStartEnd =
                    { start = newStart, end = newEnd }
            in
            ( { model | startEnd = newStartEnd, now = time }, Cmd.none )

        Tick time ->
            ( { model | now = time }, Cmd.none )

        UpdateDays amount ptype ->
            case ptype of
                Start ->
                    ( updateDays amount model.startEnd.start
                        |> setStartPoint model
                    , Cmd.none
                    )

                End ->
                    ( updateDays amount model.startEnd.end
                        |> setEndPoint model
                    , Cmd.none
                    )

        UpdateHours amount ptype ->
            case ptype of
                Start ->
                    ( updateHours amount model.startEnd.start
                        |> setStartPoint model
                    , Cmd.none
                    )

                End ->
                    ( updateHours amount model.startEnd.end
                        |> setEndPoint model
                    , Cmd.none
                    )

        UpdateMinutes amount ptype ->
            case ptype of
                Start ->
                    ( updateMinutes amount model.startEnd.start
                        |> setStartPoint model
                    , Cmd.none
                    )

                End ->
                    ( updateMinutes amount model.startEnd.end
                        |> setEndPoint model
                    , Cmd.none
                    )

        OpenLink ->
            ( model
            , StartEnd.createLink model.url model.startEnd
                |> E.string
                |> Ports.openLink
            )

        CopyLink ->
            ( model
            , StartEnd.createLink model.url model.startEnd
                |> E.string
                |> Ports.copyString
            )



-- LoadLink ->
--     ( model
--     , StartEnd.createLink model.url model.startEnd
--         |> Browser.Navigation.load
--     )
-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewNow model.zone model.now
        , div
            [ class "start-end" ]
            [ div [ class "start-end__row" ]
                [ div [ class "start-end__label" ] [ text "Start" ]
                , div [ class "start-end__point" ] [ viewDays Start model.startEnd.start.day ]
                , div [ class "start-end__point" ] [ viewHours Start model.startEnd.start.hour ]
                , div [ class "start-end__point" ] [ viewMinutes Start model.startEnd.start.minute ]
                ]
            , div [ class "start-end__row" ]
                [ div [ class "start-end__label" ] [ text "End" ]
                , div [ class "start-end__point" ] [ viewDays End model.startEnd.end.day ]
                , div [ class "start-end__point" ] [ viewHours End model.startEnd.end.hour ]
                , div [ class "start-end__point" ] [ viewMinutes End model.startEnd.end.minute ]
                ]
            ]
        , div []
            [ text "link"
            , br [] []
            , text (StartEnd.createLink model.url model.startEnd)
            , button [ onClick OpenLink ] [ text "open" ]
            , button [ onClick CopyLink ] [ text "copy" ]

            --, button [ onClick LoadLink ] [ text "load" ]
            ]
        ]


viewDays : PointType -> Int -> Html Msg
viewDays ptype num =
    div []
        [ text "days"
        , button [ onClick (UpdateDays -1 ptype) ] [ text "⬇" ]
        , button [ onClick (UpdateDays 1 ptype) ] [ text "⬆" ]
        , br [] []
        , input [ value (String.fromInt num) ] []
        ]


viewHours : PointType -> Int -> Html Msg
viewHours ptype num =
    div []
        [ text "hours"
        , button [ onClick (UpdateHours -1 ptype) ] [ text "⬇" ]
        , button [ onClick (UpdateHours 1 ptype) ] [ text "⬆" ]
        , button [ onClick (UpdateHours -5 ptype) ] [ text "5⬇" ]
        , button [ onClick (UpdateHours 5 ptype) ] [ text "5⬆" ]
        , br [] []
        , input [ value (String.fromInt num) ] []
        ]


viewMinutes : PointType -> Int -> Html Msg
viewMinutes ptype num =
    div []
        [ text "minutes"
        , button [ onClick (UpdateMinutes -1 ptype) ] [ text "⬇" ]
        , button [ onClick (UpdateMinutes 1 ptype) ] [ text "⬆" ]
        , button [ onClick (UpdateMinutes -5 ptype) ] [ text "5⬇" ]
        , button [ onClick (UpdateMinutes 5 ptype) ] [ text "5⬆" ]
        , br [] []
        , input [ value (String.fromInt num) ] []
        ]


viewInput : msg -> msg -> String -> Int -> Html msg
viewInput downMsg upMsg label num =
    div []
        [ text label
        , button [ onClick downMsg ] [ text "⬇" ]
        , button [ onClick upMsg ] [ text "⬆" ]
        , br [] []
        , input [ value (String.fromInt num) ] []
        ]


viewNow : Time.Zone -> Time.Posix -> Html msg
viewNow zone time =
    let
        sd =
            StartEnd.toStringPoint (StartEnd.posixToPoint zone time)
    in
    h2 [] [ text (sd.day ++ "th_" ++ sd.hour ++ ":" ++ sd.minute ++ ":" ++ sd.second) ]



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Time.every 300 Tick
