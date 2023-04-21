module Example exposing (cppDataTest,suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Col.CppData as Cpp exposing (make_cpp_data, make_fsm_row,makeFsmRowTable)
import Main exposing (update, Model, Msg(..))



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
        ,test "Test without with only states" <|
            \_ ->
                let
                    mdl = [["s0","s1","","",""]]
                in
                    Cpp.makeFsmRowTable mdl
                        |> Expect.equal "*s0            = s1\n        "
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



updateTest : Test
updateTest =
    describe "Update test (update model)"
        [ test "AddRow" <|
            \_ ->
                let
                    mdl = { tableData = [["s0","s1","e1","g1","a1"]]
                          , systemName = Cpp.defaultName
                          }
                    expectedMdl = { tableData = [ ["s0","s1","e1","g1","a1"]
                                                , ["","","","",""]
                                                ]
                                  , systemName = Cpp.defaultName
                                  }
                in
                    update AddRow mdl |> Expect.equal expectedMdl

        ,test "DelRow" <|
            \_ ->
                let
                    mdl = { tableData = [ ["s0","s1","e1","g1","a1"]
                                                , ["","","","",""]
                                                ]
                                  , systemName = Cpp.defaultName
                                  }
                    expectedMdl = { tableData = [ ["s0","s1","e1","g1","a1"]]
                                  , systemName = Cpp.defaultName
                                  }
                in
                    update DelRow mdl |> Expect.equal expectedMdl

        ,test "DelRow Empty" <|
            \_ ->
                let
                    mdl = { tableData = []
                                  , systemName = Cpp.defaultName
                                  }
                    expectedMdl = { tableData = []
                                  , systemName = Cpp.defaultName
                                  }
                in
                    update DelRow mdl |> Expect.equal expectedMdl

        ]
