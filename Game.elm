module Game (..) where

import String exposing (length)
import Char
import Debug
import Array exposing (Array)
import Matrix exposing (Row, Matrix)


{-
| Game types definition
-}


type State
    = InProgress
    | Solved Int
    | Failed Int
    | Timeout


type alias Achievements =
    { round : Int
    , score : Int
    , level : Int
    }


initialAchievements : Achievements
initialAchievements =
    Achievements 0 0 0


type alias Board =
    { numbers : Matrix
    , solution : Row
    , width : Int
    , height : Int
    }


type alias Strategy =
    { initialGame : Game
    , createNext : Game -> Game
    , updateState : Game -> Row -> Int -> ( State, Int )
    }


type alias Game =
    { board : Board
    , input : Array Char
    , cursorPosition : Int
    , state : State
    , clock : Int
    , achievements : Achievements
    , minLevel : Int
    , maxLevel : Int
    }
