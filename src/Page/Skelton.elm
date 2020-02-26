module Page.Skelton exposing (Model, Msg, init, subscriptions, update, view)

import Env exposing (Env)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, s, top)
import Url.Parser.Query as Q



-- PROGRAM
-- MODEL


type alias Model =
    { state : State
    }


type State
    = Init
    | Loaded String
    | Success String
    | Error String
    | Waiting String


init : Env -> ( Model, Cmd Msg )
init env =
    ( Model Init
    , Cmd.none
    )



-- UPDATE


type Msg
    = None


update : Msg -> Model -> Env -> ( Model, Cmd Msg, Env )
update msg model env =
    case msg of
        None ->
            ( { model | state = Init }, Cmd.none, env )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "Hello"
        , viewState model
        ]


viewState : Model -> Html Msg
viewState model =
    case model.state of
        Init ->
            div []
                [ text "Loading..." ]

        Loaded str ->
            div []
                [ text str ]

        Success str ->
            div [] [ text str ]

        Error str ->
            div []
                [ text str ]

        Waiting str ->
            div []
                [ text str ]



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.none
