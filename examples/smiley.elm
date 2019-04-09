import Playground exposing (..)



main =
  picture
    [ circle lightYellow 200

    -- left eye
    , circle white 50
        |> moveLeft 70
        |> moveUp 50
    , circle black 10
        |> moveLeft 75
        |> moveUp 30
    , rectangle lightYellow 100 50
        |> moveLeft 70
        |> moveUp 80

    -- right eye
    , circle white 50
        |> moveRight 70
        |> moveUp 50
    , circle black 10
        |> moveRight 65
        |> moveUp 30
    , rectangle lightYellow 100 50
        |> moveRight 70
        |> moveUp 80

    -- mouth
    , oval black 180 40
        |> moveDown 100
    , oval lightYellow 180 40
        |> moveDown 90
    ]
