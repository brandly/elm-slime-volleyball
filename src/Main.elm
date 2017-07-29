module Main exposing (..)

import AnimationFrame
import Color exposing (Color)
import Color.Convert
import Debug exposing (log)
import Html exposing (Html, button, div, h1, text)
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


type alias Vector =
    { x : Float
    , y : Float
    }


distance : Vector -> Vector -> Float
distance a b =
    (b.x - a.x) ^ 2 + (b.y - a.y) ^ 2 |> sqrt


type alias Coords =
    { x : Int
    , y : Int
    }


type alias Player =
    { name : String
    , position : Coords
    , leftKey : KeyCode
    , rightKey : KeyCode
    , jumpKey : KeyCode
    , color : Color
    , velocity : Vector
    }


type alias Game =
    Coords


type alias Wall =
    { height : Int
    , width : Int
    }


type alias Model =
    { player1 : Player
    , player2 : Player
    , ui : Ui
    , game : Game
    , wall : Wall
    }


initialUi : Ui
initialUi =
    { windowSize = ( 500, 500 )
    , pressedKeys = Set.empty
    , screen = StartScreen
    }


initialModel : Model
initialModel =
    { player1 = Player "cool" (Coords 10 0) 65 68 87 Color.blue { x = 0, y = 0 }
    , player2 = Player "lame" (Coords 500 0) 37 39 38 Color.red { x = 0, y = 0 }
    , ui = initialUi
    , game = { x = 500, y = 500 }
    , wall = { height = 50, width = 10 }
    }


freshGame : Model -> Model
freshGame { ui, game } =
    let
        ui_ =
            { ui
                | screen = PlayScreen
                , pressedKeys = Set.empty
            }
    in
    { initialModel | ui = ui_, game = game }


type Msg
    = ResizeWindow Size
    | Tick Time
    | KeyChange Bool KeyCode
    | TimeSecond Time
    | StartGame


update : Msg -> Model -> ( Model, Cmd Msg )
update action ({ ui, player1, player2, game } as model) =
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
                maybeJump player =
                    if keyPressed player.jumpKey ui.pressedKeys && player.position.y == 0 then
                        jump player
                    else
                        player

                updatePlayer player =
                    player
                        |> maybeJump
                        |> (\n -> applyKeysToPlayerPosition ui.pressedKeys n game)

                player1_ =
                    updatePlayer player1

                player2_ =
                    updatePlayer player2
            in
            ( { model | player1 = player1_, player2 = player2_ }, Cmd.none )

        StartGame ->
            ( freshGame model, Cmd.none )

        TimeSecond _ ->
            --({ model | secondsPassed = model.secondsPassed+1 }, Cmd.none)
            ( model, Cmd.none )


applyKeysToPlayerPosition : PressedKeys -> Player -> Game -> Player
applyKeysToPlayerPosition pressedKeys player game =
    let
        player_ =
            applyVelocityToPlayer player

        leftPressed =
            keyPressed player.leftKey pressedKeys

        rightPressed =
            keyPressed player.rightKey pressedKeys

        position =
            player_.position

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

        position_ =
            { position | x = x }
    in
    { player_ | position = position_ }


applyVelocityToPlayer : Player -> Player
applyVelocityToPlayer ({ position, velocity } as player) =
    let
        y_ =
            max 0 (player.position.y + round velocity.y)

        position_ =
            { position | y = y_ }

        vy_ =
            if position_.y > 0 then
                velocity.y - 1.0
            else
                velocity.y

        velocity_ =
            { velocity | y = vy_ }
    in
    { player | velocity = velocity_, position = position_ }


handleKeyChange : Bool -> KeyCode -> Model -> Model
handleKeyChange pressed keycode ({ ui, player1, player2 } as model) =
    let
        fn =
            if pressed then
                Set.insert
            else
                Set.remove

        pressedKeys_ =
            fn keycode ui.pressedKeys
    in
    case ui.screen of
        PlayScreen ->
            let
                ui_ =
                    { ui | pressedKeys = pressedKeys_ }
            in
            { model | ui = ui_ }

        _ ->
            model


jumpVelocity : Float
jumpVelocity =
    10.0


jump : Player -> Player
jump ({ position, velocity } as player) =
    let
        vy =
            if position.y == 0 then
                jumpVelocity
            else
                velocity.y

        velocity_ =
            { velocity | y = vy }
    in
    { player | velocity = velocity_ }


keyPressed : KeyCode -> PressedKeys -> Bool
keyPressed keycode pressedKeys =
    Set.member keycode pressedKeys



--NoOp ->
--  (model, Cmd.none)
-- SUBSCRIPTIONS


type alias Size =
    ( Int, Int )


type alias PressedKeys =
    Set KeyCode


type alias Ui =
    { windowSize : Size
    , pressedKeys : PressedKeys
    , screen : Screen
    }


type Screen
    = StartScreen
    | PlayScreen
    | GameoverScreen


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
    (case ui.screen of
        StartScreen ->
            [ window, seconds ]

        PlayScreen ->
            [ window ] ++ keys ++ animation

        GameoverScreen ->
            [ window ] ++ keys
    )
        |> Sub.batch


initialWindowSizeCommand : Cmd Msg
initialWindowSizeCommand =
    Task.perform (\{ width, height } -> ResizeWindow ( width, height )) Window.size



-- VIEW
-- TODO: how do we scale this based on window/game size? move into Player model?


ballRadius : Int
ballRadius =
    10


playerRadius : Int
playerRadius =
    25


playerSize : Coords
playerSize =
    { x = playerRadius * 2
    , y = playerRadius
    }


containerWidth : Int
containerWidth =
    720


view : Model -> Html Msg
view ({ ui, game } as model) =
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
        [ case ui.screen of
            StartScreen ->
                renderStartScreen model

            PlayScreen ->
                renderPlayScreen model

            GameoverScreen ->
                div [] [ text "game over" ]
        ]


renderStartScreen : Model -> Html Msg
renderStartScreen model =
    let
        topMargin =
            model.game.y // 2 - 48
    in
    div
        []
        [ renderHeader
        , button
            [ onClick StartGame
            , style
                [ ( "display", "block" )
                , ( "margin", toString topMargin ++ "px auto 0" )
                , ( "background", Color.Convert.colorToHex Color.blue )
                , ( "color", Color.Convert.colorToHex Color.white )
                , ( "border", "none" )
                , ( "font-size", "36px" )
                , ( "padding", "12px 24px" )
                , ( "border-radius", "3px" )
                , ( "letter-spacing", "2px" )
                ]
            ]
            [ text "start" ]
        ]


renderHeader : Html Msg
renderHeader =
    h1 [ style [ ( "text-align", "center" ) ] ] [ text "~ slime volleyball ~" ]


renderPlayScreen : Model -> Html Msg
renderPlayScreen { game, player1, player2, wall } =
    div
        []
        [ renderHeader
        , renderWall wall game
        , renderPlayer player1 game
        , renderPlayer player2 game
        ]


renderWall : Wall -> Game -> Html Msg
renderWall wall game =
    let
        left =
            game.x // 2 - wall.width - 2
    in
    div
        [ style
            [ ( "width", toString wall.width ++ "px" )
            , ( "height", toString wall.height ++ "px" )
            , ( "background", "black" )
            , ( "position", "absolute" )
            , ( "left", toString left ++ "px" )
            , ( "bottom", "0" )
            ]
        ]
        []


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
            , ( "background", Color.Convert.colorToHex player.color )
            , ( "width", toString playerSize.x ++ "px" )
            , ( "height", toString playerSize.y ++ "px" )
            , ( "border-top-left-radius", toString (playerSize.x * 2) ++ "px" )
            , ( "border-top-right-radius", toString (playerSize.x * 2) ++ "px" )
            ]
        ]
        []
