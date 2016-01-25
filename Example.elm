module Example (..) where

import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (..)
import String exposing (length)
import Char
import Debug


-- MODEL


type alias Model =
    {}


init : ( Model, Effects Action )
init =
    ( Model, Effects.none )



-- UPDATE


type Action
    = Noop


update : Action -> Model -> ( Model, Effects Action )
update message model =
    case message of
        Noop ->
            ( model, Effects.none )



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
    div
        []
        [ text "Hello World!" ]
