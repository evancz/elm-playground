import Playground exposing (..)



memory = 0


draw window mouse keyboard memory =
  [ circle lightYellow 200
  , moveRight 80 (moveUp 50 (circle white 50))
  , moveRight 75 (moveUp 40 (circle black 10))
  , moveRight 80 (moveUp 100 (rectangle lightYellow 100 (50 + 50 * sin (degrees memory))))
  , moveLeft 80 (moveUp 50 (circle white 50))
  , moveLeft 75 (moveUp 40 (circle black 10))
  , moveLeft 80 (moveUp 100 (rectangle lightYellow 100 (50 + 50 * sin (degrees memory))))
  , moveDown 100 (oval black 180 40)
  , moveDown 90 (oval lightYellow 180 40)
  ]



-- WHEN THINGS CHANGE


whenMouseIsClicked _ memory =
  memory


whenKeyIsPressed _ key memory =
  memory


whenTimePasses _ timePassed memory =
  memory + 1



-- START EVERYTHING!


main =
  play memory draw whenMouseIsClicked whenKeyIsPressed whenTimePasses