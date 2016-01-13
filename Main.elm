
import StartApp
import Addition
import Time


app =
  StartApp.start
    { init = Addition.init
    , update = Addition.update
    , view = Addition.view
    , inputs = [
        -- Every second signal clock tick
        Signal.map (\_ -> Addition.Tick) (Time.fps 1)
      ]
    }


main =
    app.html