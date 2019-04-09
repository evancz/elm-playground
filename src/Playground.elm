module Playground exposing
  ( picture
  --
  , Shape
  , circle, oval
  , square, rectangle
  , triangle, pentagon, hexagon, octagon, polygon
  , image, words
  , move, moveUp, moveDown, moveLeft, moveRight, moveX, moveY
  , scale, rotate
  , fade
  , group
  --
  , Number
  --
  , animation
  --
  , Computer
  --
  , Mouse
  --
  , Keyboard
  , toX
  , toY
  , toXY
  --
  , Screen
  --
  , Color
  , red, orange, yellow, green, blue, purple, brown
  , lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown
  , darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown
  , white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black
  , rgb
  , lightGray, gray, darkGray
  )

{-|

# Pictures
@docs picture

# Shapes
@docs Shape, circle, oval, square, rectangle, triangle, pentagon, hexagon, octagon, polygon

# Words
@docs words

# Images
@docs image

# Move Shapes
@docs move, moveUp, moveDown, moveLeft, moveRight, moveX, moveY

# Customize Shapes
@docs scale, rotate, fade, group

# Animation
@docs animation, Computer

# Mouse
@docs Mouse

# Keyboard
@docs Keyboard, toX, toY, toXY

# Screen
@docs Screen

# Colors
@docs Color, red, orange, yellow, green, blue, purple, brown

### Light Colors
@docs lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown

### Dark Colors
@docs darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown

### Shades of Grey
@docs white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black

### Custom Colors
@docs rgb

### Alternate Spellings of Gray
@docs lightGray, gray, darkGray

-}


import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Html
import Html.Attributes as H
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Json.Decode as D
import Set
import Task



-- PICTURE


picture : List Shape -> Program () () ()
picture shapes =
  Browser.document
    { init = \_ -> ((), Cmd.none)
    , view = \_ -> { title = "Playground", body = [ render (toScreen 400 400) shapes ] }
    , update = \_ _ -> ((), Cmd.none)
    , subscriptions = \_ -> Sub.none
    }



-- COMPUTER


type alias Computer =
  { mouse : Mouse
  , keyboard : Keyboard
  , screen : Screen
  }



-- MOUSE


type alias Mouse =
  { x : Number
  , y : Number
  , down : Bool
  , click : Bool
  }


type alias Number = Float



-- KEYBOARD


-- https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values
type alias Keyboard =
  { up : Bool
  , down : Bool
  , left : Bool
  , right : Bool
  , space : Bool
  , enter : Bool
  , shift : Bool
  , backspace : Bool
  , keys : Set.Set String
  }


toX : Keyboard -> Number
toX keyboard =
  (if keyboard.right then 1 else 0) - (if keyboard.left then 1 else 0)


toY : Keyboard -> Number
toY keyboard =
  (if keyboard.up then 1 else 0) - (if keyboard.down then 1 else 0)


toXY : Keyboard -> (Number, Number)
toXY keyboard =
  let
    x = toX keyboard
    y = toY keyboard
  in
  if x /= 0 && y /= 0 then
    (x * squareRootOfTwo, y * squareRootOfTwo)
  else
    (x,y)


squareRootOfTwo : Number
squareRootOfTwo =
  sqrt 2



-- SCREEN


type alias Screen =
  { width : Number
  , height : Number
  , top : Number
  , left : Number
  , right : Number
  , bottom : Number
  }



{-- CLOCK


type Clock = Clock Time.Posix


zigzag : Number -> Number -> Number -> Clock -> Number
zigzag period lo hi (Clock posix) =


wave : Number -> Number -> Number -> Clock -> Number
wave period lo hi (Clock posix) =


-}


-- ANIMATION


animation : (Computer -> memory -> List Shape) -> (Computer -> memory -> memory) -> memory -> Program () (Model memory) Msg
animation viewMemory updateMemory initialMemory =
  let
    init () =
      ( Model E.Visible initialMemory initialComputer
      , Task.perform GotViewport Dom.getViewport
      )

    view (Model _ memory computer) =
      { title = "Playground"
      , body = [ render computer.screen (viewMemory computer memory) ]
      }

    update msg model =
      ( animationUpdate updateMemory msg model
      , Cmd.none
      )

    subscriptions (Model visibility _ _) =
      case visibility of
        E.Hidden ->
          E.onVisibilityChange VisibilityChanged

        E.Visible ->
          animationSubscriptions
  in
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


initialComputer : Computer
initialComputer =
  { mouse = Mouse 0 0 False False
  , keyboard = emptyKeyboard
  , screen = toScreen 600 600
  }



-- SUBSCRIPTIONS


animationSubscriptions : Sub Msg
animationSubscriptions =
  Sub.batch
    [ E.onResize Resized
    , E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
    , E.onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
    , E.onAnimationFrame (\_ -> Tick)
    , E.onVisibilityChange VisibilityChanged
    , E.onClick (D.succeed MouseClick)
    , E.onMouseDown (D.succeed (MouseButton True))
    , E.onMouseUp (D.succeed (MouseButton False))
    , E.onMouseMove (D.map2 MouseMove (D.field "pageX" D.float) (D.field "pageY" D.float))
    ]



-- ANIMATION HELPERS


type Model memory =
  Model E.Visibility memory Computer


type Msg
  = KeyChanged Bool String
  | Tick
  | GotViewport Dom.Viewport
  | Resized Int Int
  | VisibilityChanged E.Visibility
  | MouseMove Float Float
  | MouseClick
  | MouseButton Bool


animationUpdate : (Computer -> memory -> memory) -> Msg -> Model memory -> Model memory
animationUpdate updateMemory msg (Model vis memory computer) =
  case msg of
    Tick ->
      Model vis (updateMemory computer memory) <|
        if computer.mouse.click
        then { computer | mouse = mouseClick False computer.mouse }
        else computer

    GotViewport {viewport} ->
      Model vis memory { computer | screen = toScreen viewport.width viewport.height }

    Resized w h ->
      Model vis memory { computer | screen = toScreen (toFloat w) (toFloat h) }

    KeyChanged isDown key ->
      Model vis memory { computer | keyboard = updateKeyboard isDown key computer.keyboard }

    MouseMove pageX pageY ->
      let
        x = computer.screen.left + pageX
        y = computer.screen.top - pageY
      in
      Model vis memory { computer | mouse = mouseMove x y computer.mouse }

    MouseClick ->
      Model vis memory { computer | mouse = mouseClick True computer.mouse }

    MouseButton isDown ->
      Model vis memory { computer | mouse = mouseDown isDown computer.mouse }

    VisibilityChanged visibility ->
      Model visibility memory
        { computer
            | keyboard = emptyKeyboard
            , mouse = Mouse computer.mouse.x computer.mouse.y False False
        }



-- SCREEN HELPERS


toScreen : Float -> Float -> Screen
toScreen width height =
  { width = width
  , height = height
  , top = height / 2
  , left = -width / 2
  , right = width / 2
  , bottom = -height / 2
  }



-- MOUSE HELPERS


mouseClick : Bool -> Mouse -> Mouse
mouseClick bool mouse =
  { mouse | click = bool }


mouseDown : Bool -> Mouse -> Mouse
mouseDown bool mouse =
  { mouse | down = bool }


mouseMove : Float -> Float -> Mouse -> Mouse
mouseMove x y mouse =
  { mouse | x = x, y = y }



-- KEYBOARD HELPERS


emptyKeyboard : Keyboard
emptyKeyboard =
  { up = False
  , down = False
  , left = False
  , right = False
  , space = False
  , enter = False
  , shift = False
  , backspace = False
  , keys = Set.empty
  }


updateKeyboard : Bool -> String -> Keyboard -> Keyboard
updateKeyboard isDown key keyboard =
  let
    keys =
      if isDown then
        Set.insert key keyboard.keys
      else
        Set.remove key keyboard.keys
  in
  case key of
    " "          -> { keyboard | keys = keys, space = isDown }
    "Enter"      -> { keyboard | keys = keys, enter = isDown }
    "Shift"      -> { keyboard | keys = keys, shift = isDown }
    "Backspace"  -> { keyboard | keys = keys, backspace = isDown }
    "ArrowUp"    -> { keyboard | keys = keys, up = isDown }
    "ArrowDown"  -> { keyboard | keys = keys, down = isDown }
    "ArrowLeft"  -> { keyboard | keys = keys, left = isDown }
    "ArrowRight" -> { keyboard | keys = keys, right = isDown }
    _            -> { keyboard | keys = keys }



-- SHAPES


type Shape =
  Shape
    Number -- x
    Number -- y
    Number -- angle
    Number -- scale
    Number -- alpha
    Form


type Form
  = Circle Color Number
  | Oval Color Number Number
  | Rectangle Color Number Number
  | Ngon Color Int Number
  | Polygon Color (List (Number, Number))
  | Image Number Number String
  | Words Color String
  | Group (List Shape)


circle : Color -> Number -> Shape
circle color radius =
  Shape 0 0 0 1 1 (Circle color radius)


oval : Color -> Number -> Number -> Shape
oval color width height =
  Shape 0 0 0 1 1 (Oval color width height)


square : Color -> Number -> Shape
square color n =
  Shape 0 0 0 1 1 (Rectangle color n n)


rectangle : Color -> Number -> Number -> Shape
rectangle color width height =
  Shape 0 0 0 1 1 (Rectangle color width height)


triangle : Color -> Number -> Shape
triangle color radius =
  Shape 0 0 0 1 1 (Ngon color 3 radius)


pentagon : Color -> Number -> Shape
pentagon color radius =
  Shape 0 0 0 1 1 (Ngon color 5 radius)


hexagon : Color -> Number -> Shape
hexagon color radius =
  Shape 0 0 0 1 1 (Ngon color 6 radius)


octagon : Color -> Number -> Shape
octagon color radius =
  Shape 0 0 0 1 1 (Ngon color 8 radius)


polygon : Color -> List (Number, Number) -> Shape
polygon color points =
  Shape 0 0 0 1 1 (Polygon color points)


image : Number -> Number -> String -> Shape
image w h src =
  Shape 0 0 0 1 1 (Image w h src)


words : Color -> String -> Shape
words color string =
  Shape 0 0 0 1 1 (Words color string)


group : List Shape -> Shape
group shapes =
  Shape 0 0 0 1 1 (Group shapes)



-- TRANSFORMS


move : Number -> Number -> Shape -> Shape
move dx dy (Shape x y a s o f) =
  Shape (x + dx) (y + dy) a s o f


moveUp : Number -> Shape -> Shape
moveUp =
  moveY


moveDown : Number -> Shape -> Shape
moveDown dy (Shape x y a s o f) =
  Shape x (y - dy) a s o f


moveLeft : Number -> Shape -> Shape
moveLeft dx (Shape x y a s o f) =
  Shape (x - dx) y a s o f


moveRight : Number -> Shape -> Shape
moveRight =
  moveX


moveX : Number -> Shape -> Shape
moveX dx (Shape x y a s o f) =
  Shape (x + dx) y a s o f


moveY : Number -> Shape -> Shape
moveY dy (Shape x y a s o f) =
  Shape x (y + dy) a s o f


scale : Number -> Shape -> Shape
scale ns (Shape x y a s o f) =
  Shape x y a (s * ns) o f


rotate : Number -> Shape -> Shape
rotate da (Shape x y a s o f) =
  Shape x y (a + da) s o f


fade : Number -> Shape -> Shape
fade o (Shape x y a s _ f) =
  Shape x y a s o f



-- COLOR


{-| Represents a color.

The colors below, like `red` and `green`, come from the [Tango palette][tango].
It provides a bunch of aesthetically reasonable colors. Each color comes with a
light and dark version, so you always get a set like `lightYellow`, `yellow`,
and `darkYellow`.

[tango]: https://en.wikipedia.org/wiki/Tango_Desktop_Project
-}
type Color
  = Hex String
  | Rgb Int Int Int


{-|-}
lightYellow : Color
lightYellow =
  Hex "#fce94f"


{-|-}
yellow : Color
yellow =
  Hex "#edd400"


{-|-}
darkYellow : Color
darkYellow =
  Hex "#c4a000"


{-|-}
lightOrange : Color
lightOrange =
  Hex "#fcaf3e"


{-|-}
orange : Color
orange =
  Hex "#f57900"


{-|-}
darkOrange : Color
darkOrange =
  Hex "#ce5c00"


{-|-}
lightBrown : Color
lightBrown =
  Hex "#9b96e"


{-|-}
brown : Color
brown =
  Hex "#c17d11"


{-|-}
darkBrown : Color
darkBrown =
  Hex "#8f5902"


{-|-}
lightGreen : Color
lightGreen =
  Hex "#ae234"


{-|-}
green : Color
green =
  Hex "#73d216"


{-|-}
darkGreen : Color
darkGreen =
  Hex "#4e9a06"


{-|-}
lightBlue : Color
lightBlue =
  Hex "#729fcf"


{-|-}
blue : Color
blue =
  Hex "#3465a4"


{-|-}
darkBlue : Color
darkBlue =
  Hex "#204a87"


{-|-}
lightPurple : Color
lightPurple =
  Hex "#ad7fa8"


{-|-}
purple : Color
purple =
  Hex "#75507b"


{-|-}
darkPurple : Color
darkPurple =
  Hex "#5c3566"


{-|-}
lightRed : Color
lightRed =
  Hex "#f2929"


{-|-}
red : Color
red =
  Hex "#cc0000"


{-|-}
darkRed : Color
darkRed =
  Hex "#a40000"


{-|-}
lightGrey : Color
lightGrey =
  Hex "#eeeec"


{-|-}
grey : Color
grey =
  Hex "#d3d7cf"


{-|-}
darkGrey : Color
darkGrey =
  Hex "#babdb6"


{-|-}
lightCharcoal : Color
lightCharcoal =
  Hex "#88a85"


{-|-}
charcoal : Color
charcoal =
  Hex "#555753"


{-|-}
darkCharcoal : Color
darkCharcoal =
  Hex "#2e3436"


{-|-}
white : Color
white =
  Hex "#FFFFFF"


{-|-}
black : Color
black =
  Hex "#000000"


-- ALTERNATE SPELLING GREYS


{-|-}
lightGray : Color
lightGray =
  Hex "#eeeec"


{-|-}
gray : Color
gray =
  Hex "#d3d7cf"


{-|-}
darkGray : Color
darkGray =
  Hex "#babdb6"





-- CUSTOM COLORS


{-| RGB stands for Red-Green-Blue. With these three parts, you can create any
color you want. For example:

    brightBlue = rgb 18 147 216
    brightGreen = rgb 119 244 8
    brightPurple = rgb 94 28 221

It can be hard to figure out what numbers to pick, so try using a color picker
like [paletton][] to find colors that look nice together. Once you find nice
colors, click on the color previews to get their RGB values.

[paletton]: http://paletton.com/
-}
rgb : Number -> Number -> Number -> Color
rgb r g b =
  Rgb (colorClamp r) (colorClamp g) (colorClamp b)


colorClamp : Number -> Int
colorClamp number =
  clamp 0 255 (round number)



-- RENDER


render : Screen -> List Shape -> Html.Html msg
render screen shapes =
  let
    w = String.fromFloat screen.width
    h = String.fromFloat screen.height
    hw = screen.right
    hh = screen.top
  in
  svg
    [ viewBox ("0 0 " ++ w ++ " " ++ h)
    , H.style "position" "fixed"
    , H.style "top" "0"
    , H.style "left" "0"
    , width "100%"
    , height "100%"
    ]
    (List.map (renderShape hw hh) shapes)



renderShape : Number -> Number -> Shape -> Svg msg
renderShape hw hh (Shape realX realY a s alpha form) =
  let
    sx = realX + hw
    sy = hh - realY
  in
  case form of
    Circle c sr ->
      Svg.circle
        (  cx (String.fromFloat sx)
        :: cy (String.fromFloat sy)
        :: r  (String.fromFloat sr)
        :: fill (renderColor c)
        :: addAlpha alpha (renderTransform sx sy a s)
        )
        []

    Oval c w h ->
      ellipse
        (  cx (String.fromFloat sx)
        :: cy (String.fromFloat sy)
        :: rx (String.fromFloat (w / 2))
        :: ry (String.fromFloat (h / 2))
        :: fill (renderColor c)
        :: addAlpha alpha (renderTransform sx sy a s)
        )
        []

    Rectangle c w h ->
      rect
        (  x (String.fromFloat (sx - w / 2))
        :: y (String.fromFloat (sy - h / 2))
        :: width (String.fromFloat w)
        :: height (String.fromFloat h)
        :: fill (renderColor c)
        :: addAlpha alpha (renderTransform sx sy a s)
        )
        []

    Ngon c n r ->
      Svg.polygon
        (  points (toNgonPoints 0 n r "")
        :: fill (renderColor c)
        :: addAlpha alpha (renderTransform sx sy a s)
        )
        []

    Polygon c ps ->
      Svg.polygon
        (  points (List.foldl addPoint "" ps)
        :: fill (renderColor c)
        :: addAlpha alpha (renderTransform sx sy a s)
        )
        []

    Image w h src ->
      Svg.image
        (  xlinkHref src
        :: x (String.fromFloat (sx - w / 2))
        :: y (String.fromFloat (sy - h / 2))
        :: width (String.fromFloat w)
        :: height (String.fromFloat h)
        :: addAlpha alpha (renderTransform sx sy a s)
        )
        []

    Words color string ->
      text_
        (  x (String.fromFloat sx)
        :: y (String.fromFloat sy)
        :: textAnchor "middle"
        :: dominantBaseline "central"
        :: fill (renderColor color)
        :: addAlpha alpha (renderTransform sx sy a s)
        )
        [ text string
        ]

    Group shapes ->
      g (addAlpha alpha (renderGroupTransform sx sy a s)) (List.map (renderShape hw hh) shapes)



-- RENDER COLOR


renderColor : Color -> String
renderColor color =
  case color of
    Hex str ->
      str

    Rgb r g b ->
      "rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"



-- ADD ALPHA


addAlpha : Number -> List (Svg.Attribute msg) -> List (Svg.Attribute msg)
addAlpha alpha attrs =
  if alpha == 1 then
    attrs
  else
    opacity (String.fromFloat (clamp 0 1 alpha)) :: attrs



-- RENDER TRANFORMS


renderTransform : Number -> Number -> Number -> Number -> List (Svg.Attribute msg)
renderTransform x y a s =
  if a == 0 then
    if s == 1 then
      []
    else
      [ transform (openScale s ++ close x y) ]
  else
    if s == 1 then
      [ transform (openRotate a ++ close x y) ]
    else
      let closer = close x y in
      [ transform (openScale s ++ closer ++ " " ++ openRotate a ++ closer) ]


openScale : Number -> String
openScale s =
 "scale(" ++ String.fromFloat s


openRotate : Number -> String
openRotate a =
  "rotate(" ++ String.fromFloat -a


close : Number -> Number -> String
close x y =
  if x == 0 && y == 0 then
    ")"
  else
    " " ++ String.fromFloat x
    ++ "," ++ String.fromFloat y
    ++ ")"



-- RENDER GROUP TRANSFORM


renderGroupTransform : Number -> Number -> Number -> Number -> List (Svg.Attribute msg)
renderGroupTransform x y a s =
  if x == 0 && y == 0 then
    renderTransform x y a s
  else
    let
      closer = String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"
      translate = "translate(" ++ closer
    in
    [ transform <|
        if a == 0 then
          if s == 1 then
            translate
          else
            translate ++ " " ++ openScale s ++ " " ++ closer
        else
          if s == 1 then
            translate ++ " " ++ openRotate a ++ " " ++ closer
          else
            translate ++ " " ++ openScale s ++ " " ++ closer ++ " " ++ openRotate a ++ " " ++ closer
    ]



-- POLYGON POINTS


addPoint : (Float, Float) -> String -> String
addPoint (x,y) str =
  str ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ " "


toNgonPoints : Int -> Int -> Float -> String -> String
toNgonPoints i n r string =
  if i == n then
    string
  else
    let
      a = turns (toFloat i / toFloat n)
      x = r * cos a
      y = r * sin a
    in
    toNgonPoints (i + 1) n r (string ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ " ")
