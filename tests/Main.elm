module Main exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    Test.describe "Test"
        [ Test.test "Test 1" (\() -> Expect.equal 0 0)
        ]
