import Time exposing (Time, inSeconds, fps)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (red)
import Window
import Mouse

-- Model

type alias Ball = {
  x: Float,
  y: Float
}

-- Update

update : (Float, (Float,Float)) -> Ball -> Ball
update (timeDelta, (x,y)) ball =
  { ball |
    x <- x,
    y <- y
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
  Signal.foldp update startingBall input

input : Signal (Time, (Float,Float))
input = Signal.sampleOn timeDelta (Signal.map2 (,) timeDelta relativeMouse)

relativeMouse : Signal (Float,Float)
relativeMouse =
  Signal.map2 (\(x,y) (w,h) ->
    (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
  ) Mouse.position Window.dimensions

timeDelta : Signal Time
timeDelta = Signal.map inSeconds (fps 40)
