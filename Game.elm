import Time exposing (Time, inSeconds, fps)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (red)

-- Model

type alias Ball = {
  x: Float,
  y: Float
}

-- Update

update : Float -> Ball -> Ball
update timeDelta ball =
  { ball |
    x <- ball.x + timeDelta*10
  }

-- View

view : Ball -> Element
view ball =
  container 200 200 middle <|
  collage 200 200 [
    circle 5
      |> filled red
      |> move (ball.x, ball.y)
  ]



startingBall = Ball 0 0

main : Signal Element
main = 
  Signal.map view gameState

gameState : Signal Ball
gameState =
  Signal.foldp update startingBall timeDelta

timeDelta : Signal Time
timeDelta = Signal.map inSeconds (fps 40)
