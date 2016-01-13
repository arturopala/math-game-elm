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
    = NewCharInput Int Char
    | KeyPressed Int



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
    div
        [ class "row solution"
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
        haveFocus =
            if cursorPosition == position then
                autofocus True
            else
                (Html.Attributes.attribute "data-nofocus" "")

        inputChangeHandler =
            on
                "input"
                targetValue
                (\t ->
                    Signal.message
                        address
                        (t
                            |> String.toList
                            |> List.head
                            |> Maybe.withDefault '?'
                            |> NewCharInput position
                        )
                )

        keyPreesHandler = Html.Events.onKeyDown address (\code -> KeyPressed code)

        valueOrPlaceHolder char =
            if (Char.isDigit char) then
                value (String.fromChar char)
            else
                value ""
    in
        span
            [ class "cell solution" ]
            [ input
                [ type' "text"
                , classList
                    [ ( "noselect", True )
                    , ( "wrong", character /= solution )
                    ]
                , if showSolution then
                    (value (String.fromChar solution))
                  else
                    (valueOrPlaceHolder character)
                , haveFocus
                , maxlength 1
                , tabindex (100 - position)
                , inputChangeHandler
                , keyPreesHandler
                ]
                []
            ]
