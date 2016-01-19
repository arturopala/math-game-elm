module BasicAdditionGameLogic (..) where

import String exposing (length)
import Char
import Debug
import Game exposing (..)
import Array exposing (Array)
import Matrix exposing (Row, Matrix)


timefactor =
    2


minLevel =
    2


maxLevel =
    9


strategy : Strategy
strategy =
    { initialGame = initialGame
    , createNext = createNext
    , updateState = updateState
    }


initialMatrix : Matrix
initialMatrix =
    Matrix.seed minLevel


initialGame : Game
initialGame =
    let
        achievements = Game.initialAchievements
    in
        createGame initialMatrix { achievements | level = minLevel }


createNext : Game -> Game
createNext game =
    let
        n = rem game.achievements.round (maxLevel - minLevel)

        level = min (foundLevel game.achievements.round game.minLevel) game.maxLevel

        numbers =
            Matrix.transformN n (Matrix.seed level)

        achievements = game.achievements
    in
        createGame numbers { achievements | level = level }


foundLevel : Int -> Int -> Int
foundLevel round level =
    let
        limit =
            [1..(level - minLevel + 2)]
                |> List.map (\n -> n * n)
                |> List.sum
    in
        if (round < limit) then
            level
        else
            foundLevel round (level + 1)


createGame : Matrix -> Achievements -> Game
createGame numbers achievements =
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

        seconds = height + (ceiling (timefactor * (toFloat (height * width))))
    in
        Game board input (width - 1) InProgress seconds achievements minLevel maxLevel


updateState : Game -> Row -> Int -> ( State, Int )
updateState game input clock =
    case game.state of
        Solved score ->
            ( Solved score, 0 )

        Timeout ->
            ( Timeout, 0 )

        _ ->
            let
                correctInputs = game.board.width - (countErrors game.board.solution input)

                points = (clock + correctInputs + (game.board.width * game.board.height))
            in
                if (clock == 0) then
                    ( Timeout, correctInputs )
                else if (input == game.board.solution) then
                    ( Solved points, points )
                else if (List.all Char.isDigit input) then
                    ( Failed (countErrors game.board.solution input), 0 )
                else
                    ( InProgress, 0 )


countErrors : Row -> Row -> Int
countErrors solution input =
    List.map2 (,) solution input
        |> List.filter (\( a, b ) -> a /= b)
        |> List.length
