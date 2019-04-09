import Playground exposing (..)


main =
  animation view update
    { x = 0
    , y = 0
    , angle = 0
    }

view computer turtle =
  [ rectangle blue computer.screen.width computer.screen.height
  , image 96 96 "turtle.gif"
      |> move turtle.x turtle.y
      |> rotate turtle.angle
  , words white (String.fromFloat turtle.angle)
      |> scale 8
      |> rotate 45
  ]

update computer turtle =
  { x = turtle.x + toY computer.keyboard * cos (degrees turtle.angle)
  , y = turtle.y + toY computer.keyboard * sin (degrees turtle.angle)
  , angle = turtle.angle - toX computer.keyboard
  }
