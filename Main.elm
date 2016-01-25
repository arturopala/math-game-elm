module Main (..) where

import StartApp
import Example


app =
    StartApp.start
        { init = Example.init
        , update = Example.update
        , view = Example.view
        , inputs = []
        }


main =
    app.html
