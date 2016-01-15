module Main (..) where

import StartApp
import AdditionGame
import Time
import Keyboard


app =
    StartApp.start
        { init = AdditionGame.init
        , update = AdditionGame.update
        , view = AdditionGame.view
        , inputs =
            [ -- Every second signal clock tick
              Signal.map (\_ -> AdditionGame.Tick) (Time.fps 1)
            , -- Catch key presses
              Signal.map (\code -> AdditionGame.KeyPressed code) Keyboard.presses
            , -- Catch arrow press
              Signal.map AdditionGame.arrowAsAction Keyboard.arrows
            ]
        }


main =
    app.html
