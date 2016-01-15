module CharRow (..) where

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import String


-- MODEL


type alias Model =
    List Char



-- VIEW


view : Model -> Html
view model =
    div
        [ class "row" ]
        (List.map charView model)


charView : Char -> Html
charView character =
    let
        digit = (String.fromChar character)
    in
        span
            [ class ("cell v" ++ digit) ]
            [ text digit ]
