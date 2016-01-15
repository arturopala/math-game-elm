module Matrix (..) where

import String
import Char


type alias Row =
    List Char


type alias Matrix =
    List Row


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
    (List.drop 1 a) ++ [ Maybe.withDefault '0' (List.head b) ]


left : Row -> Row -> Row
left a b =
    (List.drop 1 a) ++ [ Maybe.withDefault '0' (List.head (List.reverse b)) ]


right : Row -> Row -> Row
right a b =
    (Maybe.withDefault '0' (List.head b)) :: (List.take ((List.length a) - 1) a)
