module AdditionGame (..) where

import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (..)
import String exposing (length)
import Array exposing (Array)
import Matrix exposing (Row, Matrix)
import CharRow
import InputRow
import Char
import Debug


-- CONFIG


timefactor =
    2


waitPeriod =
    7



-- MODEL


type GameState
    = InProgress
    | Solved Int
    | Failed Int
    | Timeout


type alias Achievements =
    { round : Int
    , score : Int
    }


type alias Board =
    { numbers : Matrix
    , solution : Row
    , width : Int
    , height : Int
    }


type alias Model =
    { board : Board
    , input : Array Char
    , cursorPosition : Int
    , state : GameState
    , clock : Int
    , achievements : Achievements
    }


seed : Matrix
seed =
    [ 12345, 23456, 34567, 45678, 56789 ]
        |> List.map Matrix.split


init : ( Model, Effects Action )
init =
    ( createNextModel (Achievements 0 0), Effects.none )


createNextModel : Achievements -> Model
createNextModel achievements =
    let
        numbers =
            Matrix.transformN achievements.round seed
    in
        createModel numbers achievements


createModel : Matrix -> Achievements -> Model
createModel numbers achievements =
    let
        solution =
            numbers
                |> List.map Matrix.join
                |> List.sum
                |> Matrix.split

        _ = Debug.log "" (Matrix.join solution)

        width = List.length solution

        height = List.length numbers

        board = Board numbers solution width height

        input = Array.repeat width ' '

        seconds = ceiling (timefactor * (toFloat (height * width)))
    in
        Model board input (width - 1) InProgress seconds achievements



-- UPDATE


type Action
    = Noop
    | KeyPressed Int
    | ArrowLeft
    | ArrowRight
    | Tick
    | NextRound


arrowAsAction : { x : Int, y : Int } -> Action
arrowAsAction { x, y } =
    if x == 1 then
        ArrowRight
    else if x == -1 then
        ArrowLeft
    else
        Noop


update : Action -> Model -> ( Model, Effects Action )
update message model =
    case message of
        KeyPressed code ->
            let
                character = Char.fromCode code

                digit = code - 48
            in
                if (digit >= 0 && digit <= 9) then
                    updateModelWithNewInput model character
                else if code == 127 then
                    updateModelWithNewInput model '?'
                else
                    ( model, Effects.none )

        ArrowRight ->
            ( moveCursorRight model, Effects.none )

        ArrowLeft ->
            ( moveCursorLeft model, Effects.none )

        Tick ->
            updateModelWithNewClock model (model.clock - 1)

        NextRound ->
            let
                achievements = model.achievements
            in
                ( createNextModel
                    { achievements
                        | round = model.achievements.round + 1
                    }
                , Effects.none
                )

        _ ->
            ( model, Effects.none )


updateModelWithNewInput : Model -> Char -> ( Model, Effects Action )
updateModelWithNewInput model character =
    let
        position = model.cursorPosition

        newinput = updateInput model.input position character

        isdeleted = character == '?'

        newposition =
            (if isdeleted then
                position
             else
                position - 1
            )
                % model.board.width

        ( newstate, earned ) =
            updateState
                model
                (Array.toList newinput)
                model.clock

        achievements = model.achievements

        newachievements =
            { achievements | score = achievements.score + earned }
    in
        case model.state of
            Solved score ->
                ( model, Effects.none )

            Timeout ->
                ( model, Effects.none )

            _ ->
                ( { model
                    | input = newinput
                    , cursorPosition = newposition
                    , state = newstate
                    , achievements = newachievements
                    , clock =
                        case newstate of
                            Solved _ ->
                                0

                            _ ->
                                model.clock
                  }
                , Effects.none
                )


updateModelWithNewClock : Model -> Int -> ( Model, Effects Action )
updateModelWithNewClock model clock =
    let
        achievements = model.achievements

        ( newstate, earned ) =
            updateState
                model
                (Array.toList model.input)
                clock

        newachievements =
            { achievements | score = achievements.score + earned }

        newmodel =
            if (clock == (-waitPeriod)) then
                createNextModel
                    { achievements
                        | round = achievements.round + 1
                    }
            else
                { model
                    | clock = clock
                    , state = newstate
                    , achievements = newachievements
                }
    in
        ( newmodel, Effects.none )


updateState : Model -> Row -> Int -> ( GameState, Int )
updateState model input clock =
    case model.state of
        Solved score ->
            ( Solved score, 0 )

        Timeout ->
            ( Timeout, 0 )

        _ ->
            let
                correctInputs = model.board.width - (countErrors model.board.solution input)
            in
                if (clock == 0) then
                    ( Timeout, correctInputs )
                else if (input == model.board.solution) then
                    ( Solved (clock + correctInputs), (clock + correctInputs) )
                else if (List.all Char.isDigit input) then
                    ( Failed (countErrors model.board.solution input), 0 )
                else
                    ( InProgress, 0 )


moveCursorLeft : Model -> Model
moveCursorLeft model =
    { model
        | cursorPosition = (model.cursorPosition - 1) % model.board.width
    }


moveCursorRight : Model -> Model
moveCursorRight model =
    { model
        | cursorPosition = (model.cursorPosition + 1) % model.board.width
    }


updateInput : Array Char -> Int -> Char -> Array Char
updateInput input pos char =
    if (Char.isDigit char) then
        Array.set pos char input
    else
        Array.set pos ' ' input


countErrors : Row -> Row -> Int
countErrors solution input =
    List.map2 (,) solution input
        |> List.filter (\( a, b ) -> a /= b)
        |> List.length



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
    let
        numberRows = viewBoard address model

        inputRow = viewInputRow address model

        achievementsPanel =
            div
                [ class "achievements" ]
                [ span
                    []
                    [ text "Round" ]
                , span
                    [ class "round" ]
                    [ text (toString (model.achievements.round + 1)) ]
                , span
                    []
                    [ text "Score" ]
                , span
                    [ class "score" ]
                    [ text (toString model.achievements.score) ]
                , span
                    [ class "buttons" ]
                    [ i
                        [ class "fa fa-fast-forward"
                        , onClick address NextRound
                        ]
                        []
                    ]
                ]

        statePanel =
            div
                [ class "state" ]
                [ gameStateInfo model ]

        exercisePanel =
            div
                [ class "exercise" ]
                (numberRows
                    ++ [ inputRow
                       , div [ class "mark" ] [ text "+" ]
                       ]
                )
    in
        div
            [ classList
                [ ( "game", True )
                , ( (classForState model.state), True )
                ]
            ]
            [ achievementsPanel
            , statePanel
            , exercisePanel
            ]


gameStateInfo : Model -> Html
gameStateInfo model =
    case model.state of
        Solved score ->
            span
                []
                [ text "Solved! "
                , span
                    [ class "score" ]
                    [ text ("+" ++ (toString score)) ]
                , text " points! Next for "
                , span
                    [ class "clock" ]
                    [ text (toString (waitPeriod + model.clock)) ]
                , text " secs"
                ]

        Timeout ->
            span
                []
                [ text "Time is over, try next for "
                , span
                    [ class "clock" ]
                    [ text (toString (waitPeriod + model.clock)) ]
                , text " secs"
                ]

        InProgress ->
            span
                []
                [ span
                    [ class "clock" ]
                    [ text
                        (model.clock
                            |> toString
                            |> (String.padLeft 2 '0')
                        )
                    ]
                , span
                    []
                    [ text " secs left ..." ]
                ]

        Failed errors ->
            span
                []
                [ span
                    [ class "clock" ]
                    [ text
                        (model.clock
                            |> toString
                            |> (String.padLeft 2 '0')
                        )
                    ]
                , span
                    []
                    [ text " secs left, correct " ]
                , span
                    [ class "errors" ]
                    [ text (toString errors) ]
                , span
                    []
                    [ text " errors!" ]
                ]


classForState : GameState -> String
classForState state =
    case state of
        InProgress ->
            "inprogress"

        Solved score ->
            "solved"

        Failed items ->
            "wrong"

        Timeout ->
            "timeout"


viewBoard : Signal.Address Action -> Model -> List Html
viewBoard address model =
    List.map CharRow.view model.board.numbers


viewInputRow : Signal.Address Action -> Model -> Html
viewInputRow address model =
    InputRow.view
        (Signal.forwardTo address inputRowAction)
        (InputRow.Model model.board.solution model.input model.cursorPosition True (model.state == Timeout))


inputRowAction : InputRow.Action -> Action
inputRowAction a =
    case a of
        InputRow.Noop ->
            Noop
