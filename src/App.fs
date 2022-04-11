module App

open System
open Browser.Dom
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop

module Canvas =

    let element: HTMLCanvasElement = !! document.getElementById "canvas"
    let ctx = element.getContext_2d()

    let width = element.width
    let height = element.height

    let drawRect color rect =
        ctx.fillStyle <- U3.Case1 color
        ctx.fillRect rect

    let drawCircle color (x, y, radius, startAngle, endAngle) =
        ctx.beginPath()
        ctx.arc(x, y, radius, startAngle, endAngle)
        ctx.fillStyle <- U3.Case1 color
        ctx.fill()

    let drawText text color font position =
        ctx.fillStyle <- U3.Case1 color
        ctx.font <- font
        ctx.fillText(text, (fst position), (snd position))

module Keyboard =

    let mutable keysPressed = Set.empty<string>

    let key x =
        if keysPressed.Contains x then
            1
        else
            0

    let update (e: KeyboardEvent, pressed) =
        let op =
            if pressed then
                Set.add
            else
                Set.remove
        keysPressed <- op e.key keysPressed

    let leftControlsPressed() = (key "w", key "s")
    let rightControlsPressed() = (key "ArrowUp", key "ArrowDown")
    let spacePressed() = key " "

    let init() =
        document.addEventListener("keydown", fun e -> update(!!e, true))
        document.addEventListener("keyup", fun e -> update(!!e, false))

module PongGame =

    type PaddleElement =
        { x: float
          y: float
          width: float
          height: float }

    type BallElement =
        { x: float
          y: float
          width: float
          height: float
          speed: float
          angle: float }

    type GameStatus =
        { scoreLeft: int
          scoreRight: int
          active: bool }

    let canMove direction (paddle: PaddleElement) =
        match direction with
        | 1, _ -> paddle.y > 0.
        | _, 1 -> paddle.y + paddle.height < Canvas.height
        | _ -> false

    let move direction paddle =
        if paddle |> canMove direction then
            match direction with
            | 1, _ -> { paddle with y = paddle.y - 5. }
            | _, 1 -> { paddle with y = paddle.y + 5. }
            | _ -> paddle
        else
            paddle

    type Collision =
        | None
        | Top
        | Bottom
        | Left
        | Right
        | LeftPaddle
        | RightPaddle

    let checkCollision (leftPaddle: PaddleElement) (rightPaddle: PaddleElement) (ball: BallElement) =
        let hitTop = ball.y <= 0.
        let hitBottom = ball.y + ball.height >= Canvas.height
        let hitLeft = ball.x <= leftPaddle.x && ((ball.y >= leftPaddle.y && ball.y <= leftPaddle.y + leftPaddle.height) |> not)
        let hitRight = ball.x + ball.width >= rightPaddle.x + rightPaddle.width && ((ball.y >= rightPaddle.y && ball.y <= rightPaddle.y + rightPaddle.height) |> not)
        let hitLeftPaddle = ball.x <= leftPaddle.x + leftPaddle.width && ball.y >= leftPaddle.y && ball.y <= leftPaddle.y + leftPaddle.height
        let hitRightPaddle = ball.x + ball.width >= rightPaddle.x && ball.y >= rightPaddle.y && ball.y <= rightPaddle.y + rightPaddle.height
        match (hitTop, hitBottom, hitLeft, hitRight, hitLeftPaddle, hitRightPaddle) with
        | (true, _, _, _, _, _) -> Top
        | (_, true, _, _, _, _) -> Bottom
        | (_, _, true, _, _, _) -> Left
        | (_, _, _, true, _, _) -> Right
        | (_, _, _, _, true, _) -> LeftPaddle
        | (_, _, _, _, _, true) -> RightPaddle
        | _ -> None

    let calculateAngle (paddle: PaddleElement) hitRightPaddle determineAngle ball =
        let relativeIntersectY = (paddle.y + (paddle.height / 2.)) - ball.y
        let normalizedRelativeIntersectionY = (relativeIntersectY / (paddle.height / 2.))
        if normalizedRelativeIntersectionY = 0. && hitRightPaddle then
            Math.PI
        else
            normalizedRelativeIntersectionY |> determineAngle

    let collision leftPaddle rightPaddle ball =
        match ball |> checkCollision leftPaddle rightPaddle with
        | None -> ball.angle
        | Top | Bottom -> -ball.angle
        | Left | Right -> ball.angle
        | LeftPaddle -> ball |> calculateAngle leftPaddle false (fun intersection -> intersection * (5. * Math.PI / 12.)) // Max. bounce = 75°
        | RightPaddle -> ball |> calculateAngle rightPaddle true (fun intersection -> Math.PI - intersection * (5. * Math.PI / 12.))

    let moveBall angle ball =
        { x = ball.x + ball.speed * cos angle
          y = ball.y + ball.speed * -sin angle
          width = ball.width
          height = ball.height
          speed = ball.speed + 0.005
          angle = angle }

    let checkGameStatus leftPaddle rightPaddle ball gameStatus =
        match ball |> checkCollision leftPaddle rightPaddle with
        | Left ->
            { gameStatus with scoreRight = gameStatus.scoreRight + 1; active = false }
        | Right ->
            { gameStatus with scoreLeft = gameStatus.scoreLeft + 1; active = false }
        | _ ->
            gameStatus

    let render (w, h) (leftPaddle: PaddleElement) (rightPaddle: PaddleElement) (ball: BallElement) gameStatus  =
        (0., 0., w, h) |> Canvas.drawRect "black"
        (leftPaddle.x, leftPaddle.y, leftPaddle.width, leftPaddle.height) |> Canvas.drawRect "white"
        (rightPaddle.x, rightPaddle.y, rightPaddle.width, rightPaddle.height) |> Canvas.drawRect "white"
        (w / 4., 40.) |> Canvas.drawText (string(gameStatus.scoreLeft)) "white" "30px Arial"
        (w / 1.25 - 30., 40.) |> Canvas.drawText (string(gameStatus.scoreRight)) "white" "30px Arial"
        (ball.x, ball.y, ball.width, 0., 2. * Math.PI) |> Canvas.drawCircle "yellow"
        if gameStatus.active |> not then
            (w / 2. - 230., h / 2. + 40.) |> Canvas.drawText "Press space to start" "green" "40px Lucida Console"


    let initialLeftPaddle =
        { x = 10.
          y = Canvas.height / 2. - 70. / 2.
          width = 15.
          height = 70. }

    let initialRightPaddle =
        { x = Canvas.width - 15. - 10.
          y = Canvas.height / 2. - 70. / 2.
          width = 15.
          height = 70. }

    let initialBall = 
        { x = Canvas.width / 2.
          y = Canvas.height / 2.
          width = 5.
          height = 5.
          speed = 3.
          angle = 0. }

    let initialGameStatus =
        { scoreLeft = 0
          scoreRight = 0
          active = false }

    let rec update leftPaddle rightPaddle ball gameStatus _ =
        let leftPaddle =
            if gameStatus.active then
                leftPaddle |> move (Keyboard.leftControlsPressed())
            else
                initialLeftPaddle
        let rightPaddle =
            if gameStatus.active then
                rightPaddle |> move (Keyboard.rightControlsPressed())
            else
                initialRightPaddle
        let angle =
            if gameStatus.active then
                collision leftPaddle rightPaddle ball
            else
                ball.angle
        let ball =
            if gameStatus.active then
                ball |> moveBall angle
            else
                { initialBall with angle = if angle = 0. then Math.PI else 0. }
        let gameStatus =
            if Keyboard.spacePressed() = 1 then
                { gameStatus with active = true }
            else
                gameStatus |> checkGameStatus leftPaddle rightPaddle ball
        render (Canvas.width, Canvas.height) leftPaddle rightPaddle ball gameStatus
        window.requestAnimationFrame(update leftPaddle rightPaddle ball gameStatus) |> ignore

open PongGame

Keyboard.init()
update initialLeftPaddle initialRightPaddle initialBall initialGameStatus 0.


