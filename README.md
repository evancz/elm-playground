# Elm Playground

Create pictures, animations, and games with Elm!

This is the package I wanted when I was learning programming. Start by putting shapes on screen and work up to making games. I hope this package will be fun for a broad range of ages and backgrounds!


## Pictures

A picture is a list of shapes. For example, this picture combines a brown rectangle and a green circle to make a tree:

```elm
import Playground exposing (..)

main =
  picture
    [ rectangle brown 40 200
    , circle green 100
        |> moveUp 100
    ]
```

Play around to get familiar with all the different shapes and transformations in the library.


## Animations

An animation is a list of shapes that changes over time. For example, here is a spinning triangle:

```elm
import Playground exposing (..)

main =
  animation view

view clock =
  [ triangle orange 50
      |> rotate (spin 8 clock)
  ]
```

It will do a full spin every 8 seconds.

Maybe try making a car with spinning octogons as wheels? Try using `wave` to move things back-and-forth? Try using `zigzag` to fade things in-and-out?


## Games

A game lets you use input from the mouse and keyboard to change your picture. For example, here is a square that moves around based on the arrow keys:

```elm
import Playground exposing (..)

main =
  game view update (0,0)

view computer (x,y) =
  [ square blue 40
      |> move x y
  ]

update computer (x,y) =
  ( x + toX computer.keyboard
  , y + toY computer.keyboard
  )
```

Every game has three important parts:

1. `memory` - Store information. Our examples stores `(x,y)` coordinates.
2. `update` - Update the memory based on mouse movements, key presses, etc. Our example moves the `(x,y)` coordinate around based on the arrow keys.
3. `view` - Turn the memory into a picture. Our example just shows one blue square at the `(x,y)` coordinate we have been tracking in memory.

When you start making fancier games, you will store fancier things in memory. There is a lot of room to develop your programming skills here: Making lists, using records, creating custom types, etc.

I started off trying to make Pong, then worked on games like Breakout and Space Invaders as I learned more and more. It was really fun, and I hope it will be for you as well!


# Share Your Results!

If you make something cool, please share a picture, GIF, or video on Twitter! Add `#elmlang` so we can find it!

And if you start using this package for teaching, please share about that the same way! I know [McMaster Outreach](http://outreach.mcmaster.ca/) has been using a previous iteration of this package to help 4th through 8th graders learn some programming, and I love seeing all the pictures and animations that have come out of that!


# Future Work

I think it would be great to develop some learning resources that use "making games" as motivation for learning lists, records, custom types, etc.

My learning path was attempting to recreate Pong, then Breakout, then Space Invaders, and then Zelda. This sequence of games started me off just thinking about math for collisions, then about using lists to track many collisions, then making things fancier to handle moving ships, and finally, working with complex characters with health and inventory. Each new game built on the ones before, introducing new concepts gradually and giving me a long time to get comfortable with them.

That is what I did, but I think people should explore this independently! Maybe you have a path that works well for their 10th graders, maybe someone else has a path that works well for their 5th graders, etc.

My instinct is that we can make it really fun to learn programming, and I am excited to hear about anyone who makes progress in that general direction. So please share your materials and results!
