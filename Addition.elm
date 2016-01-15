module Addition (..) where

import Effects exposing (Effects, map, batch, Never)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (..)
import String exposing (length)
import CharRow
import InputRow
import Array exposing (Array)
import Char
import Debug


-- CONFIG


timefactor =
    2



-- MODEL


type GameState
    = InProgress
    | Solved Int
    | Failed Int
    | Timeout


type alias Row =
    List Char


type alias Matrix =
    List Row


type alias Achievements =
    { round : Int
    , score : Int
    }


type alias Model =
    { numbers : List Int
    , solution : Row
    , width : Int
    , input : Array Char
    , cursorPosition : Int
    , state : GameState
    , clock : Int
    , achievements : Achievements
    }


init : ( Model, Effects Action )
init =
    ( createNextModel (Achievements 0 0), Effects.none )


createNextModel : Achievements -> Model
createNextModel achievements =
    let
        numbers =
            changeN achievements.round seed
                |> List.map join
    in
        createModel numbers achievements


createModel : List Int -> Achievements -> Model
createModel numbers achievements =
    let
        solution = String.toList (toString (List.sum numbers))

        width = List.length solution

        input = Array.repeat width ' '

        seconds =
            ceiling
                (timefactor
                    * ((List.length numbers)
                        + (numbers
                            |> List.map (toString >> String.length)
                            |> List.sum
                          )
                        |> toFloat
                      )
                )
    in
        Model numbers solution width input (width - 1) InProgress seconds achievements


seed : Matrix
seed =
    [ 12345, 23456, 34567, 45678, 56789 ]
        |> List.map split


rotate : Matrix -> Matrix
rotate m =
    case m of
        m1 :: (m2 :: (m3 :: (m4 :: (m5 :: [])))) ->
            [ m5, (List.reverse m1), m2, (List.reverse m3), m4 ]

        _ ->
            m


mirror : Matrix -> Matrix
mirror m =
    case m of
        m1 :: (m2 :: (m3 :: (m4 :: (m5 :: [])))) ->
            [ (List.reverse m1), m2, (List.reverse m3), m4, (List.reverse m5) ]

        _ ->
            m


translate : Matrix -> Matrix
translate m =
    case m of
        m1 :: (m2 :: (m3 :: (m4 :: (m5 :: [])))) ->
            [ (pull m1 m5)
            , (right m2 m1)
            , (left m3 m2)
            , (right m4 m3)
            , (left m5 m4)
            ]

        _ ->
            m


changeN : Int -> Matrix -> Matrix
changeN n m =
    (List.repeat n mirror)
        |> List.intersperse rotate
        |> List.intersperse translate
        |> List.take n
        |> List.foldl apply m


apply : (Matrix -> Matrix) -> Matrix -> Matrix
apply f l =
    f l


split : Int -> Row
split n =
    n |> toString >> String.toList


join : Row -> Int
join list =
    list |> String.fromList >> String.toInt >> Result.withDefault 0


pull : Row -> Row -> Row
pull a b =
    (List.drop 1 a) ++ [ Maybe.withDefault '0' (List.head b) ]


left : Row -> Row -> Row
left a b =
    (List.drop 1 a) ++ [ Maybe.withDefault '0' (List.head (List.reverse b)) ]


right : Row -> Row -> Row
right a b =
    (Maybe.withDefault '0' (List.head b)) :: (List.take ((List.length a) - 1) a)



-- UPDATE


type Action
    = Noop
    | NewCharInput Int Char
    | InputKeyPressed Int
    | Tick


update : Action -> Model -> ( Model, Effects Action )
update message model =
    case message of
        Noop ->
            ( model, Effects.none )

        InputKeyPressed code ->
            case code of
                37 ->
                    ( { model
                        | cursorPosition = (model.cursorPosition - 1) % model.width
                      }
                    , Effects.none
                    )

                39 ->
                    ( { model
                        | cursorPosition = (model.cursorPosition + 1) % model.width
                      }
                    , Effects.none
                    )

                _ ->
                    ( model, Effects.none )

        NewCharInput position char ->
            let
                newinput = updateInput model.input position char

                isdeleted = char == '?'

                newposition =
                    (if isdeleted then
                        position
                     else
                        position - 1
                    )
                        % model.width

                newstate =
                    updateState
                        model.state
                        model.solution
                        (Array.toList newinput)
                        model.clock
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
                          }
                        , Effects.none
                        )

        Tick ->
            let
                newclock =
                    model.clock - 1

                achievements = model.achievements

                newmodel =
                    if (newclock < -10) then
                        createNextModel
                            { achievements
                                | round = achievements.round + 1
                                , score = (model.achievements.score + (earnedPoints model.state))
                            }
                    else
                        { model
                            | clock = newclock
                            , state =
                                updateState
                                    model.state
                                    model.solution
                                    (Array.toList model.input)
                                    newclock
                        }
            in
                ( newmodel, Effects.none )


updateInput : Array Char -> Int -> Char -> Array Char
updateInput input pos char =
    if (Char.isDigit char) then
        Array.set pos char input
    else
        Array.set pos ' ' input


updateState : GameState -> Row -> Row -> Int -> GameState
updateState state solution input clock =
    case state of
        Solved score ->
            Solved score

        Timeout ->
            Timeout

        _ ->
            if (clock <= 0) then
                Timeout
            else if (input == solution) then
                Solved (clock + (List.length solution) - (countErrors solution input))
            else if (List.all Char.isDigit input) then
                Failed (countErrors solution input)
            else
                InProgress


earnedPoints : GameState -> Int
earnedPoints state =
    case state of
        Solved score ->
            score

        _ ->
            0


countErrors : Row -> Row -> Int
countErrors solution input =
    List.map2 (,) solution input
        |> List.filter (\( a, b ) -> a /= b)
        |> List.length



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
    let
        numberRows = viewNumberRows address model

        inputRow = viewInputRow address model
    in
        div
            [ class "game" ]
            [ div
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
                ]
            , div
                [ classList
                    [ ( "state", True )
                    , ( (classForState model.state), True )
                    ]
                ]
                [ gameStateInfo model ]
            , div
                [ classList
                    [ ( "exercise", True )
                    , ( (classForState model.state), True )
                    ]
                ]
                (numberRows
                    ++ [ inputRow
                       , div [ class "mark" ] [ text "+" ]
                       ]
                )
            ]


gameStateInfo : Model -> Html
gameStateInfo model =
    case model.state of
        Solved score ->
            text
                ("Solved for "
                    ++ (toString score)
                    ++ " score!"
                )

        Timeout ->
            text
                ("Time is over, try again!")

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


viewNumberRows : Signal.Address Action -> Model -> List Html
viewNumberRows address model =
    List.map
        (CharRow.view (Signal.forwardTo address charRowAction))
        (List.map (toCharList model.width) model.numbers)


charRowAction : CharRow.Action -> Action
charRowAction a =
    case a of
        CharRow.Noop ->
            Noop


toCharList : Int -> Int -> Row
toCharList width number =
    let
        string = toString number

        remaining = width - (String.length string)

        spaces = String.repeat remaining " "

        row = spaces ++ string
    in
        String.toList row


viewInputRow : Signal.Address Action -> Model -> Html
viewInputRow address model =
    InputRow.view
        (Signal.forwardTo address inputRowAction)
        (InputRow.Model model.solution model.input model.cursorPosition True (model.state == Timeout))


inputRowAction : InputRow.Action -> Action
inputRowAction a =
    case a of
        InputRow.NewCharInput pos char ->
            NewCharInput pos char

        InputRow.KeyPressed code ->
            InputKeyPressed code
