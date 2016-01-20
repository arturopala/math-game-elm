module CharRow (..) where

import Html exposing (..)
import Html.Attributes exposing (class, attribute)
import Html.Events exposing (..)
import String
import Char


-- MODEL


type alias Model =
    List Char


type Action
    = Click Char



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
    div
        [ class "row" ]
        (List.map (charView address) model)


charView : Signal.Address Action -> Char -> Html
charView address character =
    let
        digit = (String.fromChar character)
    in
        span
            [ class
                ("cell v" ++ digit)
            , if (Char.isDigit character) then
                onClick address (Click character)
              else
                attribute "data-noclick" ""
            ]
            [ text digit ]
