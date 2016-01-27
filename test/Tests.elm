module Tests (..) where

import ElmTest exposing (suite, equals, Test)
import MatrixSpec


all : Test
all =
    suite
        "A math game test suite"
        [ MatrixSpec.all ]
