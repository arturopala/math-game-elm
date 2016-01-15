module CharRow (..) where

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import String


-- MODEL


type alias Model =
    List Char



-- UPDATE


type Action
    = Noop


update : Action -> Model -> Model
update message model =
    case message of
        Noop ->
            model



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
            [ class ("cell v" ++ digit) ]
            [ text digit ]
