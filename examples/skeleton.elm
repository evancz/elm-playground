import Playground exposing (..)



memory = 42


draw window mouse keyboard memory =
  []



-- WHEN THINGS CHANGE


whenMouseIsClicked _ memory =
  memory


whenKeyIsPressed _ key memory =
  memory


whenTimePasses _ timePassed memory =
  memory



-- START EVERYTHING!


main =
  play memory draw whenMouseIsClicked whenKeyIsPressed whenTimePasses