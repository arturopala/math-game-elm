module InputRow (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import String
import Array exposing (Array)
import Char


-- MODEL


type alias Model =
    { solution : List Char
    , input : Array Char
    , cursorPosition : Int
    , showErrors : Bool
    , showSolution : Bool
    }



-- UPDATE


type Action
    = Noop



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
    div
        [ class "row input"
        ]
        (List.map
            (inputFieldView address model.cursorPosition model.showErrors model.showSolution)
            (List.map2
                (,)
                (Array.toIndexedList model.input)
                model.solution
            )
        )


inputFieldView : Signal.Address Action -> Int -> Bool -> Bool -> ( ( Int, Char ), Char ) -> Html
inputFieldView address cursorPosition showErrors showSolution ( ( position, character ), solution ) =
    let
        valueOrPlaceHolder char =
            if (Char.isDigit char) then
                (String.fromChar char)
            else
                ""

        content =
            if showSolution then
                (String.fromChar solution)
            else
                (valueOrPlaceHolder character)
    in
        span
            [ classList
                [ ( "cell", True )
                , ( "input", True )
                , ( "focus", cursorPosition == position )
                , ( "wrong", character /= solution )
                ]
            ]
            [ text content ]
