module Example exposing (cppDataTest,suite,plantUmlTest)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Col.CppData as Cpp exposing (make_cpp_data, make_fsm_row,makeFsmRowTable)
import Main exposing (update, Model, Msg(..))
import Col.PlantUml as PU

sampleData : PU.PlantUmlData
sampleData =
    { name = "SampleData"
    , transitionTable =
          [ { startState = "A"
            , endState = "B"
            , event = "event1"
            , guard = "guard1"
            , action = "action1"
            , lineNr = 1
            }
          , { startState = "A"
            , endState = ""
            , event = "eventI"
            , guard = "guardI"
            , action = "actionI"
            , lineNr = 0
            }
          , { startState = "X"
            , endState = "C"
            , event = "event2"
            , guard = "guard2"
            , action = "action2"
            , lineNr = 2
            }
          , { startState = "B"
            , endState = "A"
            , event = "event3"
            , guard = "guard3"
            , action = "action3"
            , lineNr = 3
            }
          , { startState = "B"
            , endState = "D"
            , event = "event4"
            , guard = "guard4"
            , action = "action4"
            , lineNr = 4
            }
          ,{ startState = "D"
           , endState = "X"
           , event = "event5"
           , guard = "guard5"
           , action = "action5"
           , lineNr = 5
           }
          ]
    }ww

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
                    (updated,_) = update AddRow mdl
                in
                    updated |> Expect.equal expectedMdl

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
                    (updated,_) = update DelRow mdl
                in
                    updated |> Expect.equal expectedMdl

        ,test "DelRow Empty" <|
            \_ ->
                let
                    mdl = { tableData = []
                                  , systemName = Cpp.defaultName
                                  }
                    expectedMdl = { tableData = []
                                  , systemName = Cpp.defaultName
                                  }
                    (updated,_) = update DelRow mdl
                in
                    updated |> Expect.equal expectedMdl

        ]


plantUmlTest: Test
plantUmlTest =
    describe "PlantUml tests"
        [test "Test state uniqueness" <|
             \_ ->
                 PU.uniqueStates sampleData.transitionTable |> Expect.equal ["A","B","C","D"]
        ,test "Test getting transitions" <|
             \_ ->
                 PU.getTransitions "A" sampleData.transitionTable |> Expect.equal
                    [{ action = Just "action1",
                           endState = Just "B",
                           event = Just "event1",
                           guard = Just "guard1",
                           state = "A" }
                    ,{ action = Just "actionI",
                           endState = Nothing,
                           event = Just "eventI",
                           guard = Just "guardI",
                           state = "A" }
                    ,{ action = Just "action2",
                           endState = Just "C",
                           event = Just "event2",
                           guard = Just "guard2",
                           state = "A" }
                    ,{ action = Just "action3",
                           endState = Just "A",
                           event = Just "event3",
                           guard = Just "guard3",
                           state = "A" }
                    ,{ action = Just "action4",
                           endState = Just "D",
                           event = Just "event4",
                           guard = Just "guard4",
                           state = "A" }
                    ,{ action = Just "action5",
                           endState = Just "X",
                           event = Just "event5",
                           guard = Just "guard5",
                           state = "A" }]
        ,test "Testing Transition" <|
            \_ ->
                let
                    system = PU.createSystem sampleData


                in



        ]
