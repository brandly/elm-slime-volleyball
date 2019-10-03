module Main exposing (Model, Msg(..), main, subscriptions, update, view)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Color exposing (Color)
import Html exposing (Html, button, div, h1, h2, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Set exposing (Set)
import Task
import Tuple
import Vector exposing (Vector)


type alias Flags =
    { width : Int
    , height : Int
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init =
            \{ width, height } ->
                update (ResizeWindow ( width, height )) initialModel
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


toVector : Coords -> Vector
toVector coords =
    { x = toFloat coords.x, y = toFloat coords.y }


type alias Coords =
    { x : Int
    , y : Int
    }


type alias KeyCode =
    String


type alias Controls =
    { left : KeyCode
    , right : KeyCode
    , jump : KeyCode
    }


type Controller
    = Keyboard Controls
    | AI


type Team
    = Left
    | Right


type alias Player =
    { position : Coords
    , controller : Controller
    , color : Color
    , velocity : Vector
    , score : Int
    , team : Team
    }


createPlayer : Controller -> Color -> Team -> Player
createPlayer controller color team =
    let
        x =
            if team == Left then
                containerWidth // 2 - 100

            else
                containerWidth // 2 + 100
    in
    { position = Coords x 0
    , controller = controller
    , color = color
    , velocity = { x = 0, y = 0 }
    , score = 0
    , team = team
    }


getActiveControlsForPlayer : Set KeyCode -> Ball -> Player -> ActiveControls
getActiveControlsForPlayer pressedKeys ball player =
    case player.controller of
        Keyboard controls ->
            { left = keyPressed controls.left pressedKeys
            , right = keyPressed controls.right pressedKeys
            , jump = keyPressed controls.jump pressedKeys
            }

        AI ->
            getAiActiveControls ball player


type alias Ball =
    { position : Coords
    , velocity : Vector
    , radius : Int
    }


type alias Viewport =
    Coords


type alias Wall =
    { height : Int
    , width : Int
    }


type alias Model =
    { player1 : Player
    , player2 : Player
    , ui : Ui
    , viewport : Viewport
    , wall : Wall
    , ball : Ball
    }


keyMap =
    { w = "w"
    , a = "a"
    , d = "d"
    , leftArrow = "ArrowLeft"
    , rightArrow = "ArrowRight"
    , upArrow = "ArrowUp"
    , spacebar = " "
    }


wasdControls : Controls
wasdControls =
    { left = keyMap.a
    , right = keyMap.d
    , jump = keyMap.w
    }


arrowControls : Controls
arrowControls =
    { left = keyMap.leftArrow
    , right = keyMap.rightArrow
    , jump = keyMap.upArrow
    }


initialUi : Ui
initialUi =
    { windowSize = ( 500, 500 )
    , pressedKeys = Set.empty
    , screen = StartScreen
    }


initialModel : Model
initialModel =
    { player1 =
        createPlayer AI
            Color.blue
            Left
    , player2 =
        createPlayer (Keyboard arrowControls)
            Color.red
            Right
    , ui = initialUi
    , viewport = { x = 500, y = 500 }
    , wall = { height = 50, width = 10 }
    , ball = initialBall
    }


initialBall : Ball
initialBall =
    { position = Coords (containerWidth // 2) 200
    , velocity = Vector 0 0
    , radius = 15
    }


freshGame : Model -> Model
freshGame { ui, viewport, player1, player2 } =
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
    { initialModel | ui = ui_, viewport = viewport, player1 = player1_, player2 = player2_ }


freshDrop : Model -> Model
freshDrop ({ ui } as model) =
    let
        ui_ =
            { ui | screen = PlayScreen }
    in
    { model | ui = ui_, ball = initialBall }


type Msg
    = ResizeWindow Size
    | Tick Float
    | KeyChange Bool KeyCode
    | StartGame


update : Msg -> Model -> ( Model, Cmd Msg )
update action ({ ui, player1, player2, viewport, wall, ball } as model) =
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

                viewport_ : Viewport
                viewport_ =
                    { x = width
                    , y = height
                    }
            in
            ( { model | ui = { ui | windowSize = dimensions }, viewport = viewport_ }, Cmd.none )

        KeyChange pressed keycode ->
            ( handleKeyChange pressed keycode model, Cmd.none )

        Tick _ ->
            let
                activeControls =
                    getActiveControlsForPlayer ui.pressedKeys ball

                maybeJump player =
                    if (activeControls player).jump && player.position.y == 0 then
                        jump player

                    else
                        player

                updatePlayer player =
                    player
                        |> maybeJump
                        |> (\p -> applyControlsToPlayerPosition viewport (activeControls p) p)

                wallPos : Int
                wallPos =
                    viewport.x // 2 - wall.width // 2

                incrementScore : Player -> Player
                incrementScore player =
                    let
                        onOpponentsSide =
                            if player.team == Left then
                                ball.position.x > midCourt

                            else
                                ball.position.x < midCourt

                        midCourt =
                            viewport.x // 2

                        increment =
                            hitGround && onOpponentsSide

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
                        |> incrementScore
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
                        |> incrementScore
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
                        |> applyViewportBoundariesToBall viewport

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
applyCollisionsToBall p1 p2 ball =
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
            Vector.distance vPlayerPosition vBallPosition

        didCollide =
            round gap < playerRadius + ball.radius

        unitNormal =
            Vector.unitNormal vPlayerPosition vBallPosition

        scaledNormal =
            Vector.times unitNormal (Vector.magnitude ball.velocity)

        velocity_ =
            if didCollide then
                scaledNormal

            else
                velocity
    in
    { ball | velocity = velocity_ }


applyControlsToPlayerPosition : Viewport -> ActiveControls -> Player -> Player
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
            applyViewportBoundaries game { position | x = position.x + move }

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
getAiActiveControls ball ({ team } as player) =
    let
        moveRight =
            (ball.position.x + ball.radius) > (player.position.x + playerRadius)
    in
    { left = not moveRight
    , right = moveRight
    , jump = ball.position.y < playerRadius * 2
    }


applyViewportBoundaries : Viewport -> Coords -> Int
applyViewportBoundaries game position =
    if position.x < playerRadius then
        playerRadius

    else if position.x > game.x - playerRadius then
        game.x - playerRadius

    else
        position.x


applyViewportBoundariesToBall : Viewport -> Ball -> Ball
applyViewportBoundariesToBall viewport ({ position, radius, velocity } as ball) =
    let
        hitBoundaries =
            if position.x < radius then
                True

            else if position.x > viewport.x - radius then
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
handleKeyChange pressed keycode ({ ui } as model) =
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

        spacebarPressed =
            keyPressed keyMap.spacebar pressedKeys_

        screen_ =
            if spacebarPressed then
                PlayScreen

            else
                ui_.screen
    in
    case ui.screen of
        StartScreen ->
            { model | ui = { ui_ | screen = screen_ } }

        PlayScreen ->
            { model | ui = ui_ }

        GameoverScreen ->
            let
                model_ =
                    if spacebarPressed then
                        freshGame model

                    else
                        model
            in
            { model_ | ui = { ui_ | screen = screen_ } }


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


keyPressed : KeyCode -> Set KeyCode -> Bool
keyPressed keycode pressedKeys =
    Set.member keycode pressedKeys


type alias Size =
    ( Int, Int )


type alias Ui =
    { windowSize : Size
    , pressedKeys : Set KeyCode
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
            Browser.Events.onResize (\width height -> ResizeWindow ( width, height ))

        keys =
            [ Browser.Events.onKeyDown
                (Decode.field "key" Decode.string
                    |> Decode.map (KeyChange True)
                )
            , Browser.Events.onKeyUp
                (Decode.field "key" Decode.string
                    |> Decode.map (KeyChange False)
                )
            ]

        animation =
            [ Browser.Events.onAnimationFrameDelta Tick ]
    in
    (case ui.screen of
        StartScreen ->
            window :: keys

        PlayScreen ->
            window :: keys ++ animation

        GameoverScreen ->
            window :: keys
    )
        |> Sub.batch


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
view ({ ui } as model) =
    div
        [ style "box-sizing" "border-box"
        , style "position" "relative"
        , style "width" "100%"
        , style "max-width" (String.fromInt containerWidth ++ "px")
        , style "margin" "0 auto"
        , style "font-family" "sans-serif"
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
            model.viewport.y // 2 - 48
    in
    div
        []
        [ renderHeader
        , button
            [ onClick StartGame
            , style "display" "block"
            , style "margin" (String.fromInt topMargin ++ "px auto 0")
            , style "background" (Color.toCssString Color.blue)
            , style "color" (Color.toCssString Color.white)
            , style "border" "none"
            , style "font-size" "36px"
            , style "padding" "12px 24px"
            , style "border-radius" "3px"
            , style "letter-spacing" "2px"
            ]
            [ text "start" ]
        ]


renderHeader : Html Msg
renderHeader =
    h1 [ style "text-align" "center", style "margin" "48px 0 36px" ] [ text "~ slime volleyball ~" ]


renderPlayScreen : Model -> Html Msg
renderPlayScreen ({ player1, player2 } as model) =
    div []
        [ renderHeader
        , renderGame model
        , renderScore player1 player2
        ]


renderGame : Model -> Html Msg
renderGame { viewport, player1, player2, wall, ball } =
    div
        [ style "position" "relative"
        , style "border" "1px solid #EEE"
        , style "height" (String.fromInt viewport.y ++ "px")
        , style "width" "100%"
        ]
        (List.map (\render -> render viewport)
            [ renderWall wall
            , renderPlayer player1 ball
            , renderPlayer player2 ball
            , renderBall ball
            ]
        )


renderScore : Player -> Player -> Html Msg
renderScore p1 p2 =
    div [ style "width" "100%" ]
        [ div
            [ style "width" "50%"
            , style "display" "inline-block"
            ]
            [ renderScoreDots p1 ]
        , div
            [ style "width" "50%"
            , style "display" "inline-block"
            ]
            [ renderScoreDots p2 ]
        ]


pointsToWin : Int
pointsToWin =
    10


renderScoreDots : Player -> Html Msg
renderScoreDots player =
    let
        size =
            20

        background index =
            if index <= player.score then
                player.color

            else
                Color.white

        makeDot index =
            div
                [ style "display" "inline-block"
                , style "width" (String.fromFloat size ++ "px")
                , style "height" (String.fromFloat size ++ "px")
                , style "border" "1px solid black"
                , style "border-radius" (String.fromFloat size ++ "px")
                , style "background" (Color.toCssString (background index))
                ]
                []

        dots =
            List.range 1 pointsToWin
                |> List.map makeDot
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-between"
        , style "padding" (String.fromInt (size // 2) ++ "px")
        , style "margin" (String.fromInt size ++ "px 0")
        ]
        dots


renderWall : Wall -> Viewport -> Html Msg
renderWall wall viewport =
    let
        left =
            viewport.x // 2 - wall.width // 2
    in
    div
        [ style "width" (String.fromInt wall.width ++ "px")
        , style "height" (String.fromInt wall.height ++ "px")
        , style "background" "black"
        , style "position" "absolute"
        , style "left" (String.fromInt left ++ "px")
        , style "bottom" "0"
        ]
        []


renderPlayer : Player -> Ball -> Viewport -> Html Msg
renderPlayer player ball game =
    let
        { x } =
            player.position

        playerTop =
            gameY game playerRadius player.position

        eyeRadius =
            8

        pupilRadius =
            eyeRadius // 2

        eyePosition =
            getEyePosition eyeRadius player

        pupilPosition =
            getPupilPosition eyeRadius
                pupilRadius
                (Vector.sum eyePosition (toVector player.position))
                (toVector ball.position)
    in
    div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" (String.fromInt -playerRadius ++ "px")
        , style "transform" ("translate(" ++ String.fromInt x ++ "px, " ++ String.fromInt playerTop ++ "px)")
        , style "background" (Color.toCssString player.color)
        , style "width" (String.fromInt playerSize.x ++ "px")
        , style "height" (String.fromInt playerSize.y ++ "px")
        , style "border-top-left-radius" (String.fromInt (playerSize.x * 2) ++ "px")
        , style "border-top-right-radius" (String.fromInt (playerSize.x * 2) ++ "px")
        ]
        [ div
            [ style "position" "absolute"
            , style "left" (String.fromFloat eyePosition.x ++ "px")
            , style "top" (String.fromFloat eyePosition.y ++ "px")
            , style "width" (String.fromFloat (eyeRadius * 2) ++ "px")
            , style "height" (String.fromFloat (eyeRadius * 2) ++ "px")
            , style "background" "white"
            , style "border-radius" (String.fromFloat eyeRadius ++ "px")
            ]
            [ div
                [ style "background" "black"
                , style "width" (String.fromInt (pupilRadius * 2) ++ "px")
                , style "height" (String.fromInt (pupilRadius * 2) ++ "px")
                , style "border-radius" (String.fromInt pupilRadius ++ "px")
                , style "position" "absolute"
                , style "left" (String.fromFloat pupilPosition.x ++ "px")
                , style "bottom" (String.fromFloat pupilPosition.y ++ "px")
                ]
                []
            ]
        ]


getPupilPosition : Int -> Int -> Vector -> Vector -> Vector
getPupilPosition eyeRadius pupilRadius eyePosition ballPosition =
    let
        center =
            toVector { x = eyeRadius // 2, y = eyeRadius // 2 }

        unitNormal =
            Vector.unitNormal eyePosition ballPosition
    in
    Vector.sum center (Vector.times unitNormal (toFloat pupilRadius))


getEyePosition : Int -> Player -> Vector
getEyePosition radius player =
    let
        vPlayerPosition =
            toVector player.position

        facing =
            if player.team == Left then
                1

            else
                -1

        unitNormal =
            Vector facing 1
                |> Vector.sum vPlayerPosition
                |> Vector.unitNormal vPlayerPosition

        scalar =
            0.9

        scaledEyeVector =
            scalar
                * toFloat playerRadius
                |> Vector.times unitNormal

        eyeX =
            scaledEyeVector.x + toFloat playerRadius - toFloat radius

        eyeY =
            toFloat playerRadius - scaledEyeVector.y
    in
    { x = eyeX, y = eyeY }


renderBall : Ball -> Viewport -> Html Msg
renderBall ball game =
    let
        { x } =
            ball.position

        top =
            gameY game ball.radius ball.position

        diameter =
            ball.radius * 2
    in
    div
        [ style "position" "absolute"
        , style "top" "0"
        , style "left" (String.fromInt -ball.radius ++ "px")
        , style "transform" ("translate(" ++ String.fromInt x ++ "px, " ++ String.fromInt top ++ "px)")
        , style "background" (Color.toCssString Color.green)
        , style "width" (String.fromInt diameter ++ "px")
        , style "height" (String.fromInt diameter ++ "px")
        , style "border-radius" (String.fromInt (diameter * 2) ++ "px")
        ]
        []


gameY : Viewport -> Int -> Coords -> Int
gameY viewport radius position =
    viewport.y - position.y - radius


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
        , h2 [ style "text-align" "center" ] [ text msg ]
        ]
