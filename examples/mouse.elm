import Playground exposing (..)

main =
  game view update ()

view computer memory =
  [ circle lightPurple 30
      |> moveX computer.mouse.x
      |> moveY computer.mouse.y
      |> fade (if computer.mouse.down then 0.2 else 1)
  ]

update computer memory =
  memory
