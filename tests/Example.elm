module Example exposing (cppDataTest,suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Col.CppData as Cpp exposing (make_cpp_data, make_fsm_row,makeFsmRowTable)


suite : Test
suite =
    test "Two plus two equals 4" <|
        \_ ->  (2 + 2) |> Expect.equal 4


cppDataTest: Test
cppDataTest =
    describe "CppData tests"
        [test "MakeFsmRowTable Test" <|
             \_ ->
             let
                 mdl = [["s0","s1","e1","g1","a1"]]
             in
                 Cpp.makeFsmRowTable mdl |> Expect.equal "*s0   + event<e1>   [g1]   / (a1)   = s1\n        "
        ,test "Non start state" <|
            \_ ->
                let
                    mdl = [["","s1","e1","g1","a1"]]
                in
                    Cpp.makeFsmRowTable mdl
        |> Expect.equal ""
        ,test "Multiple rows" <|
            \_ ->
                let
                    mdl = [["s0","s1","e1","g1","a1"],["s1","s2","e2","g2","a2"]]
                in
                    Cpp.makeFsmRowTable mdl |>
                                              Expect.equal """*s0   + event<e1>   [g1]   / (a1)   = s1
        ,s1   + event<e2>   [g2]   / (a2)   = s2\n        """


        ]
