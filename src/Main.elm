module Main exposing (..)

import AnimationFrame
import Debug exposing (log)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Keyboard exposing (KeyCode)
import Set exposing (Set)
import Task
import Time exposing (Time, second)
import Tuple
import Window


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, initialWindowSizeCommand )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Coords =
    { x : Int
    , y : Int
    }


type alias Player =
    { name : String
    , position : Coords

    --, velocity
    }


type alias Model =
    { player : Player
    , ui : Ui
    }


initialUi : Ui
initialUi =
    { windowSize = ( 500, 500 )
    , pressedKeys = Set.empty
    , screen = StartScreen
    }


initialModel : Model
initialModel =
    { player = Player "cool" (Coords 10 2)
    , ui = initialUi
    }


type Msg
    = ResizeWindow Size
    | Tick Time
    | KeyChange Bool KeyCode
    | TimeSecond Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action ({ ui } as model) =
    case action of
        ResizeWindow dimensions ->
            ( { model | ui = { ui | windowSize = dimensions } }, Cmd.none )

        KeyChange pressed keycode ->
            --(handleKeyChange pressed keycode model, Cmd.none)
            ( model, Cmd.none )

        Tick delta ->
            ( model, Cmd.none )

        --StartGame ->
        --  (freshGame ui, Cmd.none)
        TimeSecond _ ->
            --({ model | secondsPassed = model.secondsPassed+1 }, Cmd.none)
            ( model, Cmd.none )



--NoOp ->
--  (model, Cmd.none)
-- SUBSCRIPTIONS


type alias Size =
    ( Int, Int )


type alias Ui =
    { windowSize : Size
    , pressedKeys : Set KeyCode
    , screen : Screen
    }


type Screen
    = StartScreen



--| PlayScreen
--| GameoverScreen


subscriptions : Model -> Sub Msg
subscriptions { ui } =
    let
        window =
            Window.resizes (\{ width, height } -> ResizeWindow ( width, height ))

        keys =
            [ Keyboard.downs (KeyChange True)
            , Keyboard.ups (KeyChange False)
            ]

        animation =
            [ AnimationFrame.diffs Tick ]

        seconds =
            Time.every Time.second TimeSecond
    in
    case ui.screen of
        StartScreen ->
            [ window, seconds ]
                --PlayScreen ->
                --  [ window ] ++ keys ++ animation
                --GameoverScreen ->
                --  [ window ] ++ keys
                |> Sub.batch


initialWindowSizeCommand : Cmd Msg
initialWindowSizeCommand =
    Task.perform (\{ width, height } -> ResizeWindow ( width, height )) Window.size



-- VIEW


playerSize : Coords
playerSize =
    { x = 50
    , y = 50
    }


containerWidth : Int
containerWidth =
    720


view : Model -> Html Msg
view model =
    let
        width =
            min containerWidth (Tuple.first model.ui.windowSize)

        height =
            toFloat width
                |> (*) 9
                |> (\n -> n / 21)
                |> round

        game : Coords
        game =
            { x = width
            , y = height
            }

        position =
            model.player.position

        top =
            game.y - position.y - playerSize.y
    in
    div
        [ style
            [ ( "box-sizing", "border-box" )
            , ( "position", "relative" )
            , ( "width", "100%" )
            , ( "max-width", toString containerWidth ++ "px" )
            , ( "height", toString height ++ "px" )
            , ( "margin", "0 auto" )
            , ( "border", "1px solid #EEE" )
            ]
        ]
        [ div
            [ style
                [ ( "position", "absolute" )
                , ( "top", "0" )
                , ( "left", "0" )
                , ( "transform", "translate(" ++ toString position.x ++ "px, " ++ toString top ++ "px)" )
                , ( "background", "blue" )
                , ( "width", toString playerSize.x ++ "px" )
                , ( "height", toString playerSize.y ++ "px" )
                ]
            ]
            []
        ]
