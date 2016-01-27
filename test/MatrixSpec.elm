module MatrixSpec (all) where

import ElmTest exposing (suite, equals, Test)
import Check.Test exposing (test, assert)
import String
import Matrix


all : Test
all =
    suite
        "A Matrix test suite"
        [ (Matrix.split 1234567890) `equals` [ '1', '2', '3', '4', '5', '6', '7', '8', '9', '0' ] ]
