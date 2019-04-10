import Playground exposing (..)



-- MAIN


main =
  game view update
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = "right"
    }


-- VIEW


view computer mario =
  let
    w = computer.screen.width
    h = computer.screen.height
    b = computer.screen.bottom
  in
  [ rectangle (rgb 174 238 238) w h
  , rectangle (rgb 74 163 41) w 100
      |> moveY b
  , image 70 70 (toGif mario)
      |> move mario.x (b + mario.y + 76)
  ]


toGif mario =
  if mario.y > 0 then
    "images/mario/jump/" ++ mario.dir ++ ".gif"
  else if mario.vx /= 0 then
    "images/mario/walk/" ++ mario.dir ++ ".gif"
  else
    "images/mario/stand/" ++ mario.dir ++ ".gif"



-- UPDATE


update computer mario =
  let
    dt = 1.666
    vx = toX computer.keyboard
    vy =
      if mario.y == 0 then
        if computer.keyboard.up then 5 else 0
      else
        mario.vy - dt / 8
    x = mario.x + dt * vx
    y = mario.y + dt * vy
  in
  { x = x
  , y = max 0 y
  , vx = vx
  , vy = vy
  , dir = if vx == 0 then mario.dir else if vx < 0 then "left" else "right"
  }
