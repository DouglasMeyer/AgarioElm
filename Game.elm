import Time exposing (Time, inSeconds, fps)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (red)
import Window
import Mouse

-- Model

type alias Blob = {
  x: Float,
  y: Float,
  size: Float,
  xv: Float,
  yv: Float
}

-- Update

update : (Float, (Float,Float)) -> Blob -> Blob
update (timeDelta, (x,y)) blob =
  blob
    |> (\blob -> {blob | xv <- 0, yv <- 0})
    |> applyMouseForce (x,y)
    |> applyVelocity timeDelta

applyMouseForce : (Float,Float) -> Blob -> Blob
applyMouseForce (x,y) blob =
  { blob |
    xv <- blob.xv + (x-blob.x),
    yv <- blob.yv + (y-blob.y)
  }

applyVelocity : Float -> Blob -> Blob
applyVelocity timeDelta blob =
  { blob |
    x <- blob.x + timeDelta * blob.xv,
    y <- blob.y + timeDelta * blob.yv
  }

-- View

view : (Int, Int) -> Blob -> Element
view (width,height) blob =
  container width height middle <|
  collage width height [
    circle blob.size
      |> filled red
      |> move (blob.x, blob.y)
  ]



startingBlob = Blob 0 0 5 0 0

main : Signal Element
main = 
  Signal.map2 view Window.dimensions <|
  Signal.foldp update startingBlob input

input : Signal (Time, (Float,Float))
input = Signal.sampleOn timeDelta (Signal.map2 (,) timeDelta relativeMouse)

relativeMouse : Signal (Float,Float)
relativeMouse =
  Signal.map2 (\(x,y) (w,h) ->
    (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
  ) Mouse.position Window.dimensions

timeDelta : Signal Time
timeDelta = Signal.map inSeconds (fps 40)
