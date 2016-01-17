module Matrix (..) where

import String
import Char


type alias Digit =
    Char


type alias Row =
    List Digit


type alias Matrix =
    List Row


base : Row
base =
    [ '1', '2', '3', '4', '5', '6', '7', '8', '9', '8', '7', '6', '5', '4', '3', '2', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]


seed : Int -> Matrix
seed size =
    [1..size]
        |> List.map (nthSeedRow size)


nthSeedRow : Int -> Int -> Row
nthSeedRow size n =
    let
        step =
            (floor
                (9.0 / (toFloat size))
            )

        offset = (n - 1) * step
    in
        base
            |> List.drop offset
            |> List.take size


transformN : Int -> Matrix -> Matrix
transformN n m =
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
    (List.drop 1 a) ++ (List.take 1 b)


left : Row -> Row -> Row
left a b =
    (List.drop 1 a) ++ (List.take 1 (List.reverse b))


right : Row -> Row -> Row
right b a =
    (List.take 1 a) ++ (List.take ((List.length b) - 1) b)


isEven : Int -> Bool
isEven i =
    (rem i 2) == 0


rotate : Matrix -> Matrix
rotate m =
    let
        size = List.length m

        last = List.drop (size - 1) m

        init = List.take (size - 1) m

        rotated = last ++ init
    in
        mirror rotated


mirror : Matrix -> Matrix
mirror m =
    m
        |> List.indexedMap (,)
        |> List.map
            (\( i, row ) ->
                if isEven i then
                    row
                else
                    (List.reverse row)
            )


translate : Matrix -> Matrix
translate m =
    let
        length = List.length m

        turns = (List.repeat length right) |> List.intersperse left

        actions =
            (if isEven length then
                turns
             else
                (pull :: turns)
            )
                |> List.take length

        rowPairs = pairs m

        tasks = List.map2 (,) actions rowPairs
    in
        tasks |> List.map execute


execute : ( Row -> Row -> Row, ( Row, Row ) ) -> Row
execute ( fx, ( r1, r2 ) ) =
    fx r1 r2


pairs : Matrix -> List ( Row, Row )
pairs m =
    let
        size = List.length m

        last = List.drop (size - 1) m

        init = List.take (size - 1) m

        snd = last ++ init
    in
        List.map2 (,) m snd
