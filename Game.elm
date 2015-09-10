import Time exposing (Time, inSeconds, fps)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (red)
import Window

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

view : (Int, Int) -> Ball -> Element
view (width,height) ball =
  container width height middle <|
  collage width height [
    circle 5
      |> filled red
      |> move (ball.x, ball.y)
  ]



startingBall = Ball 0 0

main : Signal Element
main = 
  Signal.map2 view Window.dimensions <|
  Signal.foldp update startingBall timeDelta

timeDelta : Signal Time
timeDelta = Signal.map inSeconds (fps 40)
