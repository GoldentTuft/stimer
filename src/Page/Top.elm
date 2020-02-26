module Page.Top exposing (Model, Msg, init, subscriptions, update, view)

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
    | DownDays PointType
    | UpDays PointType
    | DownHours PointType Int
    | UpHours PointType Int
    | DownMinutes PointType Int
    | UpMinutes PointType Int
    | Tick Time.Posix
    | OpenLink
    | CopyLink


type PointType
    = Start
    | End


downDays : StartEnd.Point -> StartEnd.Point
downDays point =
    let
        newDays =
            if (point.day - 1) < 1 then
                31

            else
                point.day - 1
    in
    { point | day = newDays }


upDays : StartEnd.Point -> StartEnd.Point
upDays point =
    let
        newDays =
            if (point.day + 1) > 31 then
                1

            else
                point.day + 1
    in
    { point | day = newDays }


downHours : Int -> StartEnd.Point -> StartEnd.Point
downHours step point =
    let
        newHours =
            if (point.hour - step) < 0 then
                23

            else
                point.hour - step
    in
    { point | hour = newHours }


upHours : Int -> StartEnd.Point -> StartEnd.Point
upHours step point =
    let
        newHours =
            if (point.hour + step) > 23 then
                0

            else
                point.hour + step
    in
    { point | hour = newHours }


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

        DownDays ptype ->
            case ptype of
                Start ->
                    ( downDays model.startEnd.start
                        |> setStartPoint model
                    , Cmd.none
                    )

                End ->
                    ( downDays model.startEnd.end
                        |> setEndPoint model
                    , Cmd.none
                    )

        UpDays ptype ->
            case ptype of
                Start ->
                    ( upDays model.startEnd.start
                        |> setStartPoint model
                    , Cmd.none
                    )

                End ->
                    ( upDays model.startEnd.end
                        |> setEndPoint model
                    , Cmd.none
                    )

        DownHours ptype step ->
            case ptype of
                Start ->
                    ( downHours step model.startEnd.start
                        |> setStartPoint model
                    , Cmd.none
                    )

                End ->
                    ( downHours step model.startEnd.end
                        |> setEndPoint model
                    , Cmd.none
                    )

        UpHours ptype step ->
            case ptype of
                Start ->
                    ( upHours step model.startEnd.start
                        |> setStartPoint model
                    , Cmd.none
                    )

                End ->
                    ( upHours step model.startEnd.end
                        |> setEndPoint model
                    , Cmd.none
                    )

        DownMinutes ptype step ->
            case ptype of
                Start ->
                    ( downMinutes step model.startEnd.start
                        |> setStartPoint model
                    , Cmd.none
                    )

                End ->
                    ( downMinutes step model.startEnd.end
                        |> setEndPoint model
                    , Cmd.none
                    )

        UpMinutes ptype step ->
            case ptype of
                Start ->
                    ( upMinutes step model.startEnd.start
                        |> setStartPoint model
                    , Cmd.none
                    )

                End ->
                    ( upMinutes step model.startEnd.end
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
            ]
        ]


viewDays : PointType -> Int -> Html Msg
viewDays ptype num =
    div []
        [ text "days"
        , button [ onClick (DownDays ptype) ] [ text "⬇" ]
        , button [ onClick (UpDays ptype) ] [ text "⬆" ]
        , br [] []
        , input [ value (String.fromInt num) ] []
        ]


viewHours : PointType -> Int -> Html Msg
viewHours ptype num =
    div []
        [ text "hours"
        , button [ onClick (DownHours ptype 1) ] [ text "⬇" ]
        , button [ onClick (UpHours ptype 1) ] [ text "⬆" ]
        , button [ onClick (DownHours ptype 5) ] [ text "5⬇" ]
        , button [ onClick (UpHours ptype 5) ] [ text "5⬆" ]
        , br [] []
        , input [ value (String.fromInt num) ] []
        ]


viewMinutes : PointType -> Int -> Html Msg
viewMinutes ptype num =
    div []
        [ text "minutes"
        , button [ onClick (DownMinutes ptype 1) ] [ text "⬇" ]
        , button [ onClick (UpMinutes ptype 1) ] [ text "⬆" ]
        , button [ onClick (DownMinutes ptype 5) ] [ text "5⬇" ]
        , button [ onClick (UpMinutes ptype 5) ] [ text "5⬆" ]
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
