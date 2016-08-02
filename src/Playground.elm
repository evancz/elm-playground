module Playground exposing
  ( play
  , Computer, Window, Mouse, Keyboard
  , Button, isDown, isUp
  , Key(..)
  , Shape, circle, oval, square, rectangle
  , triangle, pentagon, hexagon, octagon
  , group
  , move, moveUp, moveDown, moveLeft, moveRight
  , stretch, rotate, fade
  , lightRed, red, darkRed
  , lightOrange, orange, darkOrange
  , lightYellow, yellow, darkYellow
  , lightGreen, green, darkGreen
  , lightBlue, blue, darkBlue
  , lightPurple, purple, darkPurple
  , lightBrown, brown, darkBrown
  , black, white
  , lightGrey, grey, darkGrey
  , lightGray, gray, darkGray
  , lightCharcoal, charcoal, darkCharcoal
  )


import AnimationFrame
import Collage
import Color exposing (Color)
import Element
import Html exposing (Html)
import Html.App
import Html.Lazy as Lazy
import Keyboard
import Mouse
import Task
import Time exposing (Time)
import Transform
import Window



-- COMPUTER


type alias Computer =
  { window : Window
  , mouse : Mouse
  , keyboard : Keyboard
  }



-- WINDOW


type alias Window =
  { top : Float
  , bottom : Float
  , left : Float
  , right : Float
  }



-- BUTTONS


type Button = Button Bool


down : Button
down =
  Button True


up : Button
up =
  Button False


isDown : Button -> Bool
isDown (Button isDown) =
  isDown


isUp : Button -> Bool
isUp (Button isDown) =
  not isDown



-- MOUSE


type alias Mouse =
  { x : Float
  , y : Float
  , button : Button
  }



-- KEYS AND KEYBOARD


type alias Keyboard =
  { space : Button
  , enter : Button
  , up : Button
  , down : Button
  , left : Button
  , right : Button
  , a : Button
  , b : Button
  , c : Button
  , d : Button
  , e : Button
  , f : Button
  , g : Button
  , h : Button
  , i : Button
  , j : Button
  , k : Button
  , l : Button
  , m : Button
  , n : Button
  , o : Button
  , p : Button
  , q : Button
  , r : Button
  , s : Button
  , t : Button
  , u : Button
  , v : Button
  , w : Button
  , x : Button
  , y : Button
  , z : Button
  }


emptyKeyboard : Keyboard
emptyKeyboard =
  { space = up
  , enter = up
  , up = up
  , down = up
  , left = up
  , right = up
  , a = up
  , b = up
  , c = up
  , d = up
  , e = up
  , f = up
  , g = up
  , h = up
  , i = up
  , j = up
  , k = up
  , l = up
  , m = up
  , n = up
  , o = up
  , p = up
  , q = up
  , r = up
  , s = up
  , t = up
  , u = up
  , v = up
  , w = up
  , x = up
  , y = up
  , z = up
  }


type Key
  = Space
  | Enter
  | Up
  | Down
  | Left
  | Right
  | A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z



-- PLAY


play
  : memory
  -> (Window -> Mouse -> Keyboard -> memory -> List Shape)
  -> (Computer -> memory -> memory)
  -> (Computer -> Key -> memory-> memory)
  -> (Computer -> Time -> memory-> memory)
  -> Program Never
play memory draw whenMouseIsClicked whenKeyIsPressed whenTimePasses =
  Html.App.program
    { init = init memory
    , view = Lazy.lazy2 view draw
    , update = update whenMouseIsClicked whenKeyIsPressed whenTimePasses
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model memory =
  { computer : Computer
  , memory : memory
  }


init : memory -> ( Model memory, Cmd Msg )
init memory =
  { computer = Computer (Window 0 0 0 0) (Mouse 0 0 (Button False)) emptyKeyboard
  , memory = memory
  }
    ! [ Task.perform (\_ -> NoOp) Resize Window.size ]



-- VIEW


view : (Window -> Mouse -> Keyboard -> memory -> List Shape) -> Model memory -> Html msg
view draw {computer, memory} =
  let
    {window,mouse,keyboard} =
      computer

    width =
      round (window.right - window.left)

    height =
      round (window.top - window.bottom)
  in
    Element.toHtml <| Collage.collage width height <| List.map reform <|
      draw window mouse keyboard memory


reform : Shape -> Collage.Form
reform (Shape form) =
  form



-- SUBSCRIPTIONS


subscriptions : Model memory -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Window.resizes Resize
    , Mouse.clicks (\_ -> MouseClick)
    , Mouse.downs (\_ -> MouseDown)
    , Mouse.ups (\_ -> MouseUp)
    , Mouse.moves MouseMove
    , AnimationFrame.diffs Tick
    , Keyboard.presses KeyPress
    , Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    ]



-- UPDATE


type Msg
  = NoOp
  | Tick Time
  | MouseClick
  | MouseDown
  | MouseUp
  | MouseMove Mouse.Position
  | KeyDown Int
  | KeyUp Int
  | KeyPress Int
  | Resize Window.Size


update
  : (Computer -> memory -> memory)
  -> (Computer -> Key -> memory -> memory)
  -> (Computer -> Time -> memory -> memory)
  -> Msg
  -> Model memory
  -> ( Model memory, Cmd Msg )
update whenMouseIsClicked whenKeyIsPressed whenTimePasses msg ({computer,memory} as model) =
  case msg of
    NoOp ->
      ( model, Cmd.none )

    Tick timeDiff ->
      let
        newMemory =
          whenTimePasses computer timeDiff memory
      in
        ( if newMemory /= memory then
            { computer = computer
            , memory = newMemory
            }
          else
            model
        , Cmd.none
        )

    MouseClick ->
      ( { computer = computer
        , memory = whenMouseIsClicked computer memory
        }
      , Cmd.none
      )

    MouseDown ->
      ( changeMouse (mouseIsDown True computer.mouse) model
      , Cmd.none
      )

    MouseUp ->
      ( changeMouse (mouseIsDown False computer.mouse) model
      , Cmd.none
      )

    MouseMove {x,y} ->
      ( changeMouse (mouseMove x y computer.mouse) model
      , Cmd.none
      )

    KeyDown code ->
      ( model, Cmd.none )

    KeyUp code ->
      ( model, Cmd.none )

    KeyPress code ->
      case toKey code of
        Nothing ->
          ( model, Cmd.none )

        Just key ->
          ( { computer = computer
            , memory = whenKeyIsPressed computer key memory
            }
          , Cmd.none
          )

    Resize {width,height} ->
      ( resize width height model, Cmd.none )


mouseIsDown : Bool -> Mouse -> Mouse
mouseIsDown isDown mouse =
  { x = mouse.x
  , y = mouse.y
  , button = Button isDown
  }


mouseMove : Int -> Int -> Mouse -> Mouse
mouseMove x y mouse =
  { x = toFloat x
  , y = toFloat y
  , button = mouse.button
  }


changeMouse : Mouse -> Model memory -> Model memory
changeMouse newMouse {computer,memory} =
  let
    newComputer =
      { window = computer.window
      , mouse = newMouse
      , keyboard = computer.keyboard
      }
  in
    { computer = newComputer
    , memory = memory
    }


toKey : Int -> Maybe Key
toKey code =
  Nothing


resize : Int -> Int -> Model memory -> Model memory
resize width height {computer,memory} =
  let
    w =
      toFloat width / 2

    h =
      toFloat height / 2

    newWindow =
      { top = h, bottom = -h, left = -w, right = w }

    newComputer =
      { window = newWindow
      , mouse = computer.mouse
      , keyboard = computer.keyboard
      }
  in
    { computer = newComputer
    , memory = memory
    }



-- SHAPES


type Shape =
  Shape Collage.Form


circle : Color -> Float -> Shape
circle color radius =
  Shape (Collage.filled color (Collage.circle radius))


oval : Color -> Float -> Float -> Shape
oval color width height =
  Shape (Collage.filled color (Collage.oval width height))


square : Color -> Float -> Shape
square color dimension =
  Shape (Collage.filled color (Collage.rect dimension dimension))


rectangle : Color -> Float -> Float -> Shape
rectangle color width height =
  Shape (Collage.filled color (Collage.rect width height))


triangle : Color -> Float -> Shape
triangle color radius =
  Shape (Collage.filled color (Collage.ngon 3 radius))


-- rightTriangle : Color -> Float -> Float -> Shape


pentagon : Color -> Float -> Shape
pentagon color radius =
  Shape (Collage.filled color (Collage.ngon 5 radius))


hexagon : Color -> Float -> Shape
hexagon color radius =
  Shape (Collage.filled color (Collage.ngon 6 radius))


octagon : Color -> Float -> Shape
octagon color radius =
  Shape (Collage.filled color (Collage.ngon 8 radius))


group : List Shape -> Shape
group shapes =
  Shape (Collage.group (List.map reform shapes))



-- TRANSFORMS


move : Float -> Float -> Shape -> Shape
move x y (Shape form) =
  Shape (Collage.move (x,y) form)


moveUp : Float -> Shape -> Shape
moveUp pixels (Shape form) =
  Shape (Collage.moveY pixels form)


moveDown : Float -> Shape -> Shape
moveDown pixels (Shape form) =
  Shape (Collage.moveY -pixels form)


moveLeft : Float -> Shape -> Shape
moveLeft pixels (Shape form) =
  Shape (Collage.moveX -pixels form)


moveRight : Float -> Shape -> Shape
moveRight pixels (Shape form) =
  Shape (Collage.moveX pixels form)


stretch : Float -> Shape -> Shape
stretch factor (Shape form) =
  Shape (Collage.scale factor form)


rotate : Float -> Shape -> Shape
rotate angle (Shape form) =
  Shape (Collage.rotate (degrees angle) form)


fade : Float -> Shape -> Shape
fade percentage (Shape form) =
  Shape (Collage.alpha (1 - percentage / 100) form)



-- COLOR


{-|-}
lightRed : Color
lightRed =
  Color.lightRed


{-|-}
red : Color
red =
  Color.red


{-|-}
darkRed : Color
darkRed =
  Color.darkRed


{-|-}
lightOrange : Color
lightOrange =
  Color.lightOrange


{-|-}
orange : Color
orange =
  Color.orange


{-|-}
darkOrange : Color
darkOrange =
  Color.darkOrange


{-|-}
lightYellow : Color
lightYellow =
  Color.lightYellow


{-|-}
yellow : Color
yellow =
  Color.yellow


{-|-}
darkYellow : Color
darkYellow =
  Color.darkYellow


{-|-}
lightGreen : Color
lightGreen =
  Color.lightGreen


{-|-}
green : Color
green =
  Color.green


{-|-}
darkGreen : Color
darkGreen =
  Color.darkGreen


{-|-}
lightBlue : Color
lightBlue =
  Color.lightBlue


{-|-}
blue : Color
blue =
  Color.blue


{-|-}
darkBlue : Color
darkBlue =
  Color.darkBlue


{-|-}
lightPurple : Color
lightPurple =
  Color.lightPurple


{-|-}
purple : Color
purple =
  Color.purple


{-|-}
darkPurple : Color
darkPurple =
  Color.darkPurple


{-|-}
lightBrown : Color
lightBrown =
  Color.lightBrown


{-|-}
brown : Color
brown =
  Color.brown


{-|-}
darkBrown : Color
darkBrown =
  Color.darkBrown


{-|-}
black : Color
black =
  Color.black


{-|-}
white : Color
white =
  Color.white


{-|-}
lightGrey : Color
lightGrey =
  Color.lightGrey


{-|-}
grey : Color
grey =
  Color.grey


{-|-}
darkGrey : Color
darkGrey =
  Color.darkGrey


{-|-}
lightGray : Color
lightGray =
  Color.lightGray


{-|-}
gray : Color
gray =
  Color.gray


{-|-}
darkGray : Color
darkGray =
  Color.darkGray


{-|-}
lightCharcoal : Color
lightCharcoal =
  Color.lightCharcoal


{-|-}
charcoal : Color
charcoal =
  Color.charcoal


{-|-}
darkCharcoal : Color
darkCharcoal =
  Color.darkCharcoal