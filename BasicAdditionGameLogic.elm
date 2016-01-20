module BasicAdditionGameLogic (..) where

import String exposing (length)
import Char
import Debug
import Game exposing (..)
import Array exposing (Array)
import Matrix exposing (Row, Matrix)


timefactor =
    1


minLevel =
    3


maxLevel =
    9


strategy : Strategy
strategy =
    { initialGame = initialGame
    , createNextRound = createNextRound
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
        createGame initialMatrix { achievements | level = ( minLevel, minLevel ) }


createNextRound : Game -> Game
createNextRound game =
    let
        round = game.achievements.round + 1

        ( w, h, step ) = calculateWidthHeightAndStep round minLevel

        width = clamp game.minLevel game.maxLevel w

        height = clamp game.minLevel game.maxLevel h

        numbers =
            Matrix.transformN round (Matrix.seed2 width height step)

        achievements = game.achievements
    in
        createGame numbers { achievements | level = ( width, height ), round = round }


calculateWidthHeightAndStep : Int -> Int -> ( Int, Int, Int )
calculateWidthHeightAndStep round level =
    let
        limit =
            [1..(level - minLevel + 2)]
                |> List.map (\n -> (n + 5) * (n + 1))
                |> List.sum

        variant = rem round (level + level)

        ( dw, dh, step ) =
            case variant of
                1 ->
                    ( 0, 0, 1 )

                2 ->
                    ( 1, 0, maxLevel )

                3 ->
                    ( 0, 1, maxLevel )

                4 ->
                    ( 1, 2, level // 3 )

                5 ->
                    ( 2, 1, level // 3 )

                6 ->
                    ( -1, (min level 2), maxLevel )

                7 ->
                    ( (min level 3), -1, maxLevel )

                _ ->
                    ( 0, 0, maxLevel )
    in
        if (round < limit) then
            ( (min (level + dw) maxLevel), (min (level + dh) maxLevel), step )
        else
            calculateWidthHeightAndStep round (level + 1)


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

        sum = numbers |> List.map (\row -> List.sum (List.map Matrix.parseCharAsInt row)) |> List.sum

        board = Board numbers solution width height sum

        input = Array.repeat width ' '
    in
        Game board input (width - 1) InProgress (timefactor * sum) achievements minLevel maxLevel


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

                points = (clock + correctInputs + game.board.sum)
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
