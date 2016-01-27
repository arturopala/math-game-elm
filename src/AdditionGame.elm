module AdditionGame (..) where

import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (..)
import String exposing (length)
import Char
import Debug
import Game exposing (..)
import Array exposing (Array)
import Matrix exposing (Row, Matrix)
import CharRow
import InputRow
import BasicAdditionGameLogic


-- CONFIG


waitPeriod =
    7



-- MODEL


type alias Model =
    { game : Game
    , strategy : Strategy
    }


init : ( Model, Effects Action )
init =
    ( Model BasicAdditionGameLogic.initialGame BasicAdditionGameLogic.strategy, Effects.none )



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
    let
        { game, strategy } = model
    in
        case message of
            KeyPressed code ->
                let
                    character = Char.fromCode code

                    digit = code - 48
                in
                    if (digit >= 0 && digit <= 9) then
                        ( { model | game = updateGameWithNewInput game strategy character }, Effects.none )
                    else if code == 127 then
                        ( { model | game = updateGameWithNewInput game strategy '?' }, Effects.none )
                    else
                        ( model, Effects.none )

            ArrowRight ->
                ( { model | game = moveCursorRight game }, Effects.none )

            ArrowLeft ->
                ( { model | game = moveCursorLeft game }, Effects.none )

            Tick ->
                ( { model | game = updateGameWithNewClock game strategy (game.clock - 1) }, Effects.none )

            NextRound ->
                let
                    achievements = game.achievements
                in
                    ( { model
                        | game =
                            strategy.createNextRound game
                      }
                    , Effects.none
                    )

            _ ->
                ( model, Effects.none )


updateGameWithNewInput : Game -> Strategy -> Char -> Game
updateGameWithNewInput game strategy character =
    let
        position = game.cursorPosition

        newinput = updateInput game.input position character

        isdeleted = character == '?'

        newposition =
            (if isdeleted then
                position
             else
                position - 1
            )
                % game.board.width

        ( newstate, earned ) =
            strategy.updateState
                game
                (Array.toList newinput)
                game.clock

        achievements = game.achievements

        newachievements =
            { achievements | score = achievements.score + earned }
    in
        case game.state of
            Solved score ->
                game

            Timeout ->
                game

            _ ->
                { game
                    | input = newinput
                    , cursorPosition = newposition
                    , state = newstate
                    , achievements = newachievements
                    , clock =
                        case newstate of
                            Solved _ ->
                                0

                            _ ->
                                game.clock
                }


updateGameWithNewClock : Game -> Strategy -> Int -> Game
updateGameWithNewClock game strategy clock =
    let
        achievements = game.achievements

        ( newstate, earned ) =
            strategy.updateState
                game
                (Array.toList game.input)
                clock

        newachievements =
            { achievements | score = achievements.score + earned }

        newgame =
            if (clock == (-waitPeriod)) then
                strategy.createNextRound game
            else
                { game
                    | clock = clock
                    , state = newstate
                    , achievements = newachievements
                }
    in
        newgame


moveCursorLeft : Game -> Game
moveCursorLeft game =
    { game
        | cursorPosition = (game.cursorPosition - 1) % game.board.width
    }


moveCursorRight : Game -> Game
moveCursorRight game =
    { game
        | cursorPosition = (game.cursorPosition + 1) % game.board.width
    }


updateInput : Array Char -> Int -> Char -> Array Char
updateInput input pos char =
    if (Char.isDigit char) then
        Array.set pos char input
    else
        Array.set pos ' ' input



-- VIEW


view : Signal.Address Action -> Model -> Html
view address { game } =
    let
        numberRows = viewBoard address game

        inputRow = viewInputRow address game

        achievementsPanel =
            div
                [ class "achievements" ]
                [ span
                    []
                    [ text "Round" ]
                , span
                    [ class "round" ]
                    [ text (toString (game.achievements.round + 1)) ]
                , span
                    []
                    [ text "Score" ]
                , span
                    [ class "score" ]
                    [ text (toString game.achievements.score) ]
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
                [ gameStateInfo game ]

        exercisePanel =
            div
                [ class "board"
                , style
                    [ ( "font-size", "1rem" ) ]
                ]
                (numberRows
                    ++ [ div [ class "mark" ] [ text "+" ]
                       , inputRow
                       ]
                )
    in
        div
            [ classList
                [ ( "game", True )
                , ( (classForState game.state), True )
                ]
            ]
            [ achievementsPanel
            , statePanel
            , exercisePanel
            ]


gameStateInfo : Game -> Html
gameStateInfo game =
    case game.state of
        Solved score ->
            let
                clock = (waitPeriod + game.clock)
            in
                span
                    []
                    [ text "You got "
                    , span
                        [ class "score" ]
                        [ text ("+" ++ (toString score)) ]
                    , text " points !!! Next play for "
                    , span
                        [ class "clock" ]
                        [ text (toString clock) ]
                    , text
                        (if clock > 1 then
                            " secs"
                         else
                            " sec"
                        )
                    ]

        Timeout ->
            let
                clock = (waitPeriod + game.clock)
            in
                span
                    []
                    [ text "Time is over, try next for "
                    , span
                        [ class "clock" ]
                        [ text (toString clock) ]
                    , text
                        (if clock > 1 then
                            " secs"
                         else
                            " sec"
                        )
                    ]

        InProgress ->
            span
                []
                [ span
                    [ class "clock" ]
                    [ text
                        (game.clock
                            |> toString
                            |> (String.padLeft 3 '0')
                        )
                    ]
                , span
                    []
                    [ text
                        (if game.clock > 1 then
                            " secs left ..."
                         else
                            " sec ..."
                        )
                    ]
                ]

        Failed errors ->
            span
                []
                [ span
                    [ class "clock" ]
                    [ text
                        (game.clock
                            |> toString
                            |> (String.padLeft 3 '0')
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


classForState : State -> String
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


viewBoard : Signal.Address Action -> Game -> List Html
viewBoard address game =
    let
        emptyCount = game.maxLevel - game.board.height

        emptyRows = List.repeat emptyCount (List.repeat (game.maxLevel + 1) ' ')

        numberRows =
            game.board.numbers
                |> List.map
                    (\row ->
                        (let
                            width = List.length row

                            missing = game.maxLevel - width + 1
                         in
                            (List.repeat missing ' ') ++ row
                        )
                    )
    in
        List.map (CharRow.view (Signal.forwardTo address charRowAction)) (emptyRows ++ numberRows)


charRowAction : CharRow.Action -> Action
charRowAction a =
    case a of
        CharRow.Click character ->
            if (Char.isDigit character) then
                KeyPressed ((Matrix.parseCharAsInt character) + 48)
            else
                Noop


viewInputRow : Signal.Address Action -> Game -> Html
viewInputRow address game =
    InputRow.view
        (Signal.forwardTo address inputRowAction)
        (InputRow.Model game.board.solution game.input game.cursorPosition True (game.state == Timeout))


inputRowAction : InputRow.Action -> Action
inputRowAction a =
    case a of
        InputRow.Noop ->
            Noop
