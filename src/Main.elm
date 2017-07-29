module Main exposing (..)

import AnimationFrame
import Color exposing (Color)
import Color.Convert
import Debug exposing (log)
import Html exposing (Html, button, div, h1, h2, text)
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


divide : Vector -> Float -> Vector
divide vec scalar =
    times vec (1 / scalar)


times : Vector -> Float -> Vector
times vec scalar =
    { x = vec.x * scalar
    , y = vec.y * scalar
    }


distance : Vector -> Vector -> Float
distance a b =
    (b.x - a.x) ^ 2 + (b.y - a.y) ^ 2 |> sqrt


magnitude =
    distance (Vector 0 0)


toNormalVector : Vector -> Vector -> Vector
toNormalVector v1 v2 =
    { x = v2.x - v1.x
    , y = v2.y - v1.y
    }


toVector : Coords -> Vector
toVector coords =
    { coords | x = toFloat coords.x, y = toFloat coords.y }


type alias Coords =
    { x : Int
    , y : Int
    }


type alias Ai =
    { active : Bool
    , direction : Int -> Int -> Bool
    }


type alias Controls =
    { left : KeyCode
    , right : KeyCode
    , jump : KeyCode
    }


type alias Player =
    { name : String
    , position : Coords
    , controls : Controls
    , color : Color
    , velocity : Vector
    , score : Int
    , ai : Ai
    }


type alias Ball =
    { position : Coords
    , velocity : Vector
    , radius : Int
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
    , ball : Ball
    }


wasdControls : Controls
wasdControls =
    { left = 65
    , right = 68
    , jump = 87
    }


arrowControls : Controls
arrowControls =
    { left = 37
    , right = 39
    , jump = 38
    }


initialUi : Ui
initialUi =
    { windowSize = ( 500, 500 )
    , pressedKeys = Set.empty
    , screen = StartScreen
    }


initialModel : Model
initialModel =
    { player1 = Player "cool" (Coords (containerWidth // 2 - 100) 0) wasdControls Color.blue { x = 0, y = 0 } 0 (Ai True (>))
    , player2 = Player "lame" (Coords (containerWidth // 2 + 100) 0) arrowControls Color.red { x = 0, y = 0 } 0 (Ai False (<))
    , ui = initialUi
    , game = { x = 500, y = 500 }
    , wall = { height = 50, width = 10 }
    , ball = Ball (Coords (containerWidth // 2) 200) (Vector 0 0) 15
    }


freshGame : Model -> Model
freshGame { ui, game, player1, player2 } =
    let
        ui_ =
            { ui
                | screen = PlayScreen
                , pressedKeys = Set.empty
            }

        player1_ =
            { player1 | score = 0 }

        player2_ =
            { player2 | score = 0 }
    in
    { initialModel | ui = ui_, game = game, player1 = player1_, player2 = player2_ }


freshDrop : Model -> Model
freshDrop { ui, game, player1, player2 } =
    let
        ui_ =
            { ui
                | screen = PlayScreen
            }
    in
    { initialModel | ui = ui_, game = game, player1 = player1, player2 = player2 }


type Msg
    = ResizeWindow Size
    | Tick Time
    | KeyChange Bool KeyCode
    | TimeSecond Time
    | StartGame


update : Msg -> Model -> ( Model, Cmd Msg )
update action ({ ui, player1, player2, game, wall, ball } as model) =
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
                    if (getActiveControlsForPlayer player).jump && player.position.y == 0 then
                        jump player
                    else
                        player

                getActiveControlsForPlayer : Player -> ActiveControls
                getActiveControlsForPlayer player =
                    if player.ai.active then
                        getAiActiveControls ball player
                    else
                        { left = keyPressed player.controls.left ui.pressedKeys
                        , right = keyPressed player.controls.right ui.pressedKeys
                        , jump = keyPressed player.controls.jump ui.pressedKeys
                        }

                updatePlayer player =
                    player
                        |> maybeJump
                        |> (\p -> applyControlsToPlayerPosition game (getActiveControlsForPlayer p) p)

                wallPos : Int
                wallPos =
                    game.x // 2 - wall.width // 2

                incrementScore : (Int -> Int -> Bool) -> Player -> Player
                incrementScore operator player =
                    let
                        midCourt =
                            game.x // 2

                        increment =
                            hitGround && operator ball.position.x midCourt

                        score_ =
                            if increment then
                                player.score + 1
                            else
                                player.score
                    in
                    { player | score = score_ }

                -- TODO: seems like we could generalize collision logic here
                player1_ =
                    player1
                        |> updatePlayer
                        |> incrementScore (>)
                        |> (\p ->
                                if p.position.x + playerRadius > wallPos then
                                    let
                                        position =
                                            p.position

                                        position_ =
                                            { position | x = wallPos - playerRadius }
                                    in
                                    { p | position = position_ }
                                else
                                    p
                           )

                player2_ =
                    player2
                        |> updatePlayer
                        |> incrementScore (<)
                        |> (\p ->
                                if p.position.x - playerRadius < wallPos + wall.width then
                                    let
                                        position =
                                            p.position

                                        position_ =
                                            { position | x = wallPos + wall.width + playerRadius }
                                    in
                                    { p | position = position_ }
                                else
                                    p
                           )

                ball_ =
                    ball
                        |> applyVelocityToBall
                        |> applyCollisionsToBall player1 player2
                        |> applyGameBoundariesToBall game

                hitGround =
                    ball.radius > ball.position.y

                gameIsOver =
                    player1_.score >= pointsToWin || player2_.score >= pointsToWin

                screen_ =
                    if gameIsOver then
                        GameoverScreen
                    else
                        PlayScreen

                ui_ =
                    { ui | screen = screen_ }

                model_ =
                    { model | player1 = player1_, player2 = player2_, ball = ball_, ui = ui_ }

                model__ =
                    if hitGround then
                        freshDrop model_
                    else
                        model_
            in
            ( model__, Cmd.none )

        StartGame ->
            ( freshGame model, Cmd.none )

        TimeSecond _ ->
            --({ model | secondsPassed = model.secondsPassed+1 }, Cmd.none)
            ( model, Cmd.none )


applyVelocityToBall : Ball -> Ball
applyVelocityToBall ({ position, velocity } as ball) =
    let
        position_ =
            { position
                | x = position.x + round velocity.x
                , y = position.y + round velocity.y
            }

        velocity_ =
            { velocity | y = velocity.y - 0.3 }
    in
    { ball | position = position_, velocity = velocity_ }


applyCollisionsToBall : Player -> Player -> Ball -> Ball
applyCollisionsToBall p1 p2 ({ position } as ball) =
    ball
        |> collideWithPlayer p1
        |> collideWithPlayer p2


collideWithPlayer : Player -> Ball -> Ball
collideWithPlayer player ({ velocity } as ball) =
    let
        vPlayerPosition =
            toVector player.position

        vBallPosition =
            toVector ball.position

        gap =
            distance vPlayerPosition vBallPosition

        didCollide =
            round gap < playerRadius + ball.radius

        unitNormal =
            getUnitNormal vPlayerPosition vBallPosition

        scaledNormal =
            times unitNormal (magnitude ball.velocity)

        velocity_ =
            if didCollide then
                scaledNormal
            else
                velocity
    in
    { ball | velocity = velocity_ }


getUnitNormal : Vector -> Vector -> Vector
getUnitNormal a b =
    let
        gap =
            distance a b

        normalVector =
            toNormalVector a b
    in
    divide normalVector (magnitude normalVector)


applyControlsToPlayerPosition : Game -> ActiveControls -> Player -> Player
applyControlsToPlayerPosition game active player =
    let
        player_ =
            applyVelocityToPlayer player

        position =
            player_.position

        move =
            if active.left then
                -5
            else if active.right then
                5
            else
                0

        x =
            applyGameBoundaries game { position | x = position.x + move }

        position_ =
            { position | x = x }
    in
    { player_ | position = position_ }


type alias ActiveControls =
    { left : Bool
    , right : Bool
    , jump : Bool
    }


getAiActiveControls : Ball -> Player -> ActiveControls
getAiActiveControls ball ({ ai } as player) =
    let
        moveRight =
            ai.direction (ball.position.x - player.position.x) 15

        left_ =
            not moveRight

        right_ =
            moveRight

        jump_ =
            if ball.position.y < playerRadius * 2 then
                True
            else
                False
    in
    { left = left_
    , right = right_
    , jump = jump_
    }


applyGameBoundaries : Game -> Coords -> Int
applyGameBoundaries game position =
    if position.x < playerRadius then
        playerRadius
    else if position.x > game.x - playerRadius then
        game.x - playerRadius
    else
        position.x


applyGameBoundariesToBall : Game -> Ball -> Ball
applyGameBoundariesToBall game ({ position, radius, velocity } as ball) =
    let
        hitBoundaries =
            if position.x < radius then
                True
            else if position.x > game.x - radius then
                True
            else
                False

        x_ =
            if hitBoundaries then
                velocity.x * -1
            else
                velocity.x
    in
    { ball | velocity = { velocity | x = x_ } }


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

        ui_ =
            { ui | pressedKeys = pressedKeys_ }
    in
    case ui.screen of
        StartScreen ->
            let
                spacebar =
                    32

                screen_ =
                    if keyPressed spacebar pressedKeys_ then
                        PlayScreen
                    else
                        StartScreen
            in
            { model | ui = { ui_ | screen = screen_ } }

        PlayScreen ->
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
            [ window, seconds ] ++ keys

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


playerRadius : Int
playerRadius =
    40


playerSize : Coords
playerSize =
    { x = playerRadius * 2
    , y = playerRadius
    }


containerWidth : Int
containerWidth =
    600


view : Model -> Html Msg
view ({ ui, game } as model) =
    div
        [ style
            [ ( "box-sizing", "border-box" )
            , ( "position", "relative" )
            , ( "width", "100%" )
            , ( "max-width", toString containerWidth ++ "px" )
            , ( "margin", "0 auto" )
            , ( "font-family", "sans-serif" )
            ]
        ]
        [ case ui.screen of
            StartScreen ->
                renderStartScreen model

            PlayScreen ->
                renderPlayScreen model

            GameoverScreen ->
                renderGameOverScreen model
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
    h1 [ style [ ( "text-align", "center" ), ( "margin", "48px 0 36px" ) ] ] [ text "~ slime volleyball ~" ]


renderPlayScreen : Model -> Html Msg
renderPlayScreen ({ player1, player2 } as model) =
    div
        []
        [ renderHeader
        , renderGame model
        , renderScore player1 player2
        ]


renderGame : Model -> Html Msg
renderGame { game, player1, player2, wall, ball } =
    div
        [ style
            [ ( "position", "relative" )
            , ( "border", "1px solid #EEE" )
            , ( "height", toString game.y ++ "px" )
            , ( "width", "100%" )
            ]
        ]
        [ renderWall wall game
        , renderPlayer player1 ball game
        , renderPlayer player2 ball game
        , renderBall ball game
        ]


renderScore : Player -> Player -> Html Msg
renderScore p1 p2 =
    div
        [ style
            [ ( "width", "100%" ) ]
        ]
        [ div
            [ style
                [ ( "width", "50%" )
                , ( "display", "inline-block" )
                ]
            ]
            [ renderScoreDots p1.score ]
        , div
            [ style
                [ ( "width", "50%" )
                , ( "display", "inline-block" )
                ]
            ]
            [ renderScoreDots p2.score ]
        ]


pointsToWin : Int
pointsToWin =
    10


renderScoreDots : Int -> Html Msg
renderScoreDots score =
    let
        size =
            20

        background index =
            if index <= score then
                "black"
            else
                "white"

        makeDot index =
            div
                [ style
                    [ ( "display", "inline-block" )
                    , ( "width", toString size ++ "px" )
                    , ( "height", toString size ++ "px" )
                    , ( "border", "1px solid black" )
                    , ( "border-radius", toString size ++ "px" )
                    , ( "background", background index )
                    ]
                ]
                []

        dots =
            List.range 1 pointsToWin
                |> List.map makeDot
    in
    div
        [ style
            [ ( "display", "flex" )
            , ( "flex-direction", "row" )
            , ( "justify-content", "space-between" )
            , ( "padding", toString (size // 2) ++ "px" )
            , ( "margin", toString size ++ "px 0" )
            ]
        ]
        dots


renderWall : Wall -> Game -> Html Msg
renderWall wall game =
    let
        left =
            game.x // 2 - wall.width // 2
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


renderPlayer : Player -> Ball -> Game -> Html Msg
renderPlayer player ball game =
    let
        { x, y } =
            player.position

        playerTop =
            gameY game playerRadius player.position

        unitNormal =
            getUnitNormal (toVector player.position) (toVector ball.position)

        scalar =
            0.8

        scaledPlayerRadius =
            scalar * toFloat playerRadius

        scaledEyeVector =
            times unitNormal scaledPlayerRadius

        eyeRadius =
            8

        eyeX =
            scaledEyeVector.x + toFloat playerRadius - toFloat eyeRadius

        eyeY =
            toFloat playerRadius - scaledEyeVector.y

        pupilRadius =
            eyeRadius // 2
    in
    div
        [ style
            [ ( "position", "absolute" )
            , ( "top", "0" )
            , ( "left", toString -playerRadius ++ "px" )
            , ( "transform", "translate(" ++ toString x ++ "px, " ++ toString playerTop ++ "px)" )
            , ( "background", Color.Convert.colorToHex player.color )
            , ( "width", toString playerSize.x ++ "px" )
            , ( "height", toString playerSize.y ++ "px" )
            , ( "border-top-left-radius", toString (playerSize.x * 2) ++ "px" )
            , ( "border-top-right-radius", toString (playerSize.x * 2) ++ "px" )
            ]
        ]
        [ div
            [ style
                [ ( "position", "absolute" )
                , ( "left", toString eyeX ++ "px" )
                , ( "top", toString eyeY ++ "px" )
                , ( "width", toString (eyeRadius * 2) ++ "px" )
                , ( "height", toString (eyeRadius * 2) ++ "px" )
                , ( "background", "white" )
                , ( "border-radius", toString eyeRadius ++ "px" )
                ]
            ]
            [ div
                [ style
                    [ ( "background", "black" )
                    , ( "width", toString (pupilRadius * 2) ++ "px" )
                    , ( "height", toString (pupilRadius * 2) ++ "px" )
                    , ( "border-radius", toString pupilRadius ++ "px" )
                    , ( "position", "absolute" )
                    , ( "left", toString (toFloat eyeRadius - toFloat pupilRadius) ++ "px" )
                    , ( "bottom", toString (toFloat eyeRadius - toFloat pupilRadius) ++ "px" )
                    ]
                ]
                []
            ]
        ]


renderBall : Ball -> Game -> Html Msg
renderBall ball game =
    let
        { x, y } =
            ball.position

        top =
            gameY game ball.radius ball.position

        diameter =
            ball.radius * 2
    in
    div
        [ style
            [ ( "position", "absolute" )
            , ( "top", "0" )
            , ( "left", toString -ball.radius ++ "px" )
            , ( "transform", "translate(" ++ toString x ++ "px, " ++ toString top ++ "px)" )
            , ( "background", Color.Convert.colorToHex Color.green )
            , ( "width", toString diameter ++ "px" )
            , ( "height", toString diameter ++ "px" )
            , ( "border-radius", toString (diameter * 2) ++ "px" )
            ]
        ]
        []


gameY : Game -> Int -> Coords -> Int
gameY game radius position =
    game.y - position.y - radius


renderGameOverScreen : Model -> Html Msg
renderGameOverScreen ({ player1, player2 } as model) =
    let
        msg =
            if player1.score > player2.score then
                "wow u succ"
            else
                "p good"
    in
    div []
        [ renderPlayScreen model
        , h2 [ style [ ( "text-align", "center" ) ] ] [ text msg ]
        ]
