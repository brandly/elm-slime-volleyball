module Main exposing (..)

import AnimationFrame
import Debug exposing (log)
import Html exposing (Html, div, h1, text)
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


type alias Game =
    Coords


type alias Model =
    { player : Player
    , ui : Ui
    , game : Game
    }


initialUi : Ui
initialUi =
    { windowSize = ( 500, 500 )
    , pressedKeys = Set.empty
    , screen = StartScreen
    }


initialModel : Model
initialModel =
    { player = Player "cool" (Coords 10 0)
    , ui = initialUi
    , game = { x = 500, y = 500 }
    }


type Msg
    = ResizeWindow Size
    | Tick Time
    | KeyChange Bool KeyCode
    | TimeSecond Time


update : Msg -> Model -> ( Model, Cmd Msg )
update action ({ ui, player, game } as model) =
    case action of
        ResizeWindow dimensions ->
            let
                width =
                    min containerWidth (Tuple.first dimensions)

                height =
                    toFloat width
                        |> (*) 9
                        |> (\n -> n / 21)
                        |> round

                game : Game
                game =
                    { x = width
                    , y = height
                    }
            in
            ( { model | ui = { ui | windowSize = dimensions }, game = game }, Cmd.none )

        KeyChange pressed keycode ->
            ( handleKeyChange pressed keycode model, Cmd.none )

        Tick delta ->
            let
                leftPressed =
                    keyPressed 37 ui.pressedKeys

                rightPressed =
                    keyPressed 39 ui.pressedKeys

                move =
                    if leftPressed then
                        -5
                    else if rightPressed then
                        5
                    else
                        0

                x =
                    if position.x + move < 0 then
                        0
                    else if position.x + move + playerSize.x > game.x then
                        game.x - playerSize.x
                    else
                        position.x + move

                position =
                    player.position

                position_ =
                    { position | x = x }

                player_ =
                    { player | position = position_ }
            in
            ( { model | player = player_ }, Cmd.none )

        --StartGame ->
        --  (freshGame ui, Cmd.none)
        TimeSecond _ ->
            --({ model | secondsPassed = model.secondsPassed+1 }, Cmd.none)
            ( model, Cmd.none )


handleKeyChange : Bool -> KeyCode -> Model -> Model
handleKeyChange pressed keycode ({ ui, player } as model) =
    let
        fn =
            if pressed then
                Set.insert
            else
                Set.remove

        pressedKeys_ =
            fn keycode ui.pressedKeys

        position =
            player.position

        ui_ =
            { ui | pressedKeys = pressedKeys_ }
    in
    { model | ui = ui_ }


keyPressed : KeyCode -> Set KeyCode -> Bool
keyPressed keycode pressedKeys =
    Set.member keycode pressedKeys



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
                ++ keys
                ++ animation
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
    , y = 25
    }


containerWidth : Int
containerWidth =
    720


view : Model -> Html Msg
view { game, player } =
    div
        [ style
            [ ( "box-sizing", "border-box" )
            , ( "position", "relative" )
            , ( "width", "100%" )
            , ( "max-width", toString containerWidth ++ "px" )
            , ( "height", toString game.y ++ "px" )
            , ( "margin", "0 auto" )
            , ( "border", "1px solid #EEE" )
            , ( "font-family", "sans-serif" )
            ]
        ]
        [ h1 [ style [ ( "text-align", "center" ) ] ] [ text "~ slime volleyball ~" ]
        , renderPlayer player game
        ]


renderPlayer : Player -> Game -> Html Msg
renderPlayer player game =
    let
        { x, y } =
            player.position

        top =
            game.y - y - playerSize.y
    in
    div
        [ style
            [ ( "position", "absolute" )
            , ( "top", "0" )
            , ( "left", "0" )
            , ( "transform", "translate(" ++ toString x ++ "px, " ++ toString top ++ "px)" )
            , ( "background", "blue" )
            , ( "width", toString playerSize.x ++ "px" )
            , ( "height", toString playerSize.y ++ "px" )
            , ( "border-top-left-radius", toString (playerSize.x * 2) ++ "px" )
            , ( "border-top-right-radius", toString (playerSize.x * 2) ++ "px" )
            ]
        ]
        []
