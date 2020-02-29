module Main exposing (main)

--import API

import Browser
import Browser.Navigation as Nav
import Env exposing (Env)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import Page.Start
import Page.Top
import Route exposing (Route)
import Task
import Time
import Url
import Url.Builder



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { url : Url.Url
    , key : Nav.Key
    , page : Page
    }


type Page
    = NotFound
    | StartPage Page.Start.Model
    | TopPage Page.Top.Model


type State
    = Init
    | Waiting
    | Running
    | Finished
    | Error


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        m1 =
            Model url key NotFound

        ( m2, c1 ) =
            goTo url m1
    in
    ( m2, Cmd.batch [ c1 ] )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | StartMsg Page.Start.Msg
    | TopMsg Page.Top.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            goTo url model

        ( StartMsg subMsg, StartPage subModel ) ->
            let
                ( newModel, topCmd ) =
                    Page.Start.update subMsg subModel model.key
            in
            ( { model | page = StartPage newModel }
            , Cmd.map StartMsg topCmd
            )

        ( TopMsg subMsg, TopPage subModel ) ->
            let
                ( newModel, topCmd ) =
                    Page.Top.update subMsg subModel
            in
            ( { model | page = TopPage newModel }
            , Cmd.map TopMsg topCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )



--( model, Cmd.none )


goTo : Url.Url -> Model -> ( Model, Cmd Msg )
goTo url model =
    case Route.parse url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just Route.Top ->
            let
                ( m, c ) =
                    Page.Top.init url
            in
            ( { model | page = TopPage m }, Cmd.map TopMsg c )

        Just (Route.Start date) ->
            let
                ( m, c ) =
                    Page.Start.init date
            in
            ( { model | page = StartPage m }, Cmd.map StartMsg c )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        s1 =
            case model.page of
                StartPage subModel ->
                    Sub.map StartMsg Page.Start.subscriptions

                TopPage subModel ->
                    Sub.map TopMsg Page.Top.subscriptions

                _ ->
                    Sub.none

        s2 =
            Sub.none
    in
    Sub.batch [ s1, s2 ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "共有タイマー(Share Timer)"
    , body =
        [ viewHeadder model.url
        , case model.page of
            NotFound ->
                viewNotFound

            TopPage subModel ->
                Page.Top.view subModel
                    |> Html.map TopMsg

            StartPage subModel ->
                Page.Start.view subModel
                    |> Html.map StartMsg
        ]
    }


viewHeadder : Url.Url -> Html msg
viewHeadder url =
    div []
        [ nav [ class "header-nav" ]
            [ ul []
                [ li [] [ a [ href (Env.makeAppTop url) ] [ span [ class "header-nav__top" ] [ text "Set Timer" ] ] ]
                ]
            ]
        ]


viewNotFound : Html msg
viewNotFound =
    div []
        [ text "NotFound"
        ]
