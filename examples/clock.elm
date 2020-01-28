import Playground exposing (..)


main =
    animation view


view time =
    let
        h =
            remainderBy (24 * 60 * 60) (now time // 1000) // (60 * 60) |> toFloat

        m =
            remainderBy (60 * 60) (now time // 1000) // 60 |> toFloat

        s =
            remainderBy 60 (now time // 1000) |> toFloat
    in
    [ [ rectangle red 10 250 |> moveY 125, triangle red 15 |> moveY 250 ] |> group |> rotate (-360 / 60 * s)
    , [ rectangle blue 10 180 |> moveY 90, triangle blue 15 |> moveY 180 ] |> group |> rotate (-360 / 60 * m)
    , [ rectangle yellow 10 120 |> moveY 60, triangle yellow 15 |> moveY 120 ] |> group |> rotate (-360 / 60 * h)
    , circle brown 10
    ]
        ++ (List.range 0 11
                |> List.map
                    (\i ->
                        [ words black (String.fromInt (12 - i))
                            |> scale 3
                            |> rotate (-360 / 12 * toFloat i)
                            |> moveY 250
                        ]
                            |> group
                            |> rotate (360 / 12 * toFloat i)
                    )
           )
