module Example exposing (cppDataTest,suite,plantUmlTest)

import Debug
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Col.CppData as Cpp exposing (make_cpp_data,makeFsmRowFromMachine,makeFsmRowFromData)
import Col.ModelData exposing (Machine)
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
    }


expectedStateStr ="""A->B: event1 [guard1] / action1
A: eventI [guardI] / actionI
B->A: event2 [guard2] / action2
B->C: eventC [guardC] / actionC
"""

expectedSystemStr = """@startuml

state SampleData{
[*]-->A
A->B: event1 [guard1] / action1
A: eventI [guardI] / actionI
B->A: event3 [guard3] / action3
B->D: event4 [guard4] / action4
D-->[*]: event5 [guard5] / action5
}
@enduml
"""


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
                 machine = { name = "Test", tableData = [{ rowIndex = 0, selected = "No Special", data = { startState = Just "s0", endState = Just "s1", event = Just "e1", guard = Just "g1", action = Just "a1" }}]}
              in
                 Cpp.makeFsmRowFromMachine [] machine |> Expect.equal "*s0               +event<e1>           [g1]            / (a1)           = s1\n        "
         ,test "Test without with only states" <|
             \_ ->
                 let
                     machine = { name = "Test", tableData = [{ rowIndex = 0, selected = "No Special", data = { startState = Just "s0", endState = Just "s1", event = Nothing, guard = Nothing, action = Nothing }}]}
                 in
                     Cpp.makeFsmRowFromMachine [] machine
                         |> Expect.equal "*s0                                                                  = s1\n        "
         ,test "Non start state" <|
             \_ ->
                 let
                     machine = { name = "Test", tableData = [{ rowIndex = 0, selected = "No Special", data = { startState = Nothing, endState = Just "s1", event = Just "e1", guard = Just "g1", action = Just "a1" }}]}
                 in
                     Cpp.makeFsmRowFromMachine [] machine
         |> Expect.equal ""
         ,test "Multiple rows" <|
             \_ ->
                 let
                     machine = { name = "Test", tableData =
                         [{ rowIndex = 0, selected = "No Special", data = { startState = Just "s0", endState = Just "s1", event = Just "e1", guard = Just "g1", action = Just "a1" }}
                         ,{ rowIndex = 1, selected = "No Special", data = { startState = Just "s1", endState = Just "s2", event = Just "e2", guard = Just "g2", action = Just "a2" }}
                         ]}
                 in
                     Cpp.makeFsmRowFromMachine [] machine |>
                                              Expect.equal "*s0               +event<e1>           [g1]            / (a1)           = s1\n        ,s1               +event<e2>           [g2]            / (a2)           = s2\n        "


        ]



plantUmlTest: Test
plantUmlTest =
    describe "PlantUml tests"
        [test "Test state uniqueness" <|
             \_ ->
                 PU.uniqueStates sampleData.transitionTable |> Expect.equal ["A","B","C","D"]
        ,test "Testing Transition" <|
            \_ ->
                let
                    system = PU.createSystem sampleData
                    checkName sys = Expect.equal sampleData.name sys.name
                    checkNrStates sys = Expect.equal (List.length sys.states)  4

                in
                    Expect.all [checkName , checkNrStates ] system

        ,test "Testing converting transitions" <|
                     \_ ->
                         let
                             system = PU.createSystem sampleData
                         in
                             Debug.log "State " (PU.getStateByIndex 0 system)
                                 |> Result.fromMaybe "not an State"
                                 |> Expect.ok
        ,test "Testing get state by lineNr" <|
                     \_ ->
                         let
                             system = PU.createSystem sampleData
                         in
                             case PU.findStateByLineNr 0 system of -- 1,2 is A
                                 Just state -> Expect.equal (state.name == "A") True
                                 Nothing -> Expect.fail "Did not find state"

            ,test "Making plantuml transition string" <|
                         \_ ->
                             let
                                 allFields state = Expect.equal (PU.makeTransitionStr state
                                                                 { action = Just "action1"
                                                                 ,  endState = Just "B"
                                                                 , event = Just "event1"
                                                                 , guard = Just "guard1"
                                                                 , lineNr = 1
                                                                 , selected = Col.ModelData.NO }) "A->B: event1 [guard1] / action1\n"

                                 noEvent state = Expect.equal (PU.makeTransitionStr state
                                                                   { action = Just "action1"
                                                                   ,  endState = Just "B"
                                                                   , event = Nothing
                                                                   , guard = Just "guard1"
                                                                   , lineNr = 1
                                                                   , selected = Col.ModelData.NO }) "A->B: [guard1] / action1\n"

                                 onlyAction state = Expect.equal (PU.makeTransitionStr state
                                                                      { action = Just "action1"
                                                                      ,  endState = Just "B"
                                                                      , event = Nothing
                                                                      , guard = Nothing
                                                                      , lineNr = 1
                                                                      , selected = Col.ModelData.NO }) "A->B: / action1\n"
                                 anonymous state = Expect.equal (PU.makeTransitionStr state
                                                                   { action = Nothing
                                                                   ,  endState = Just "B"
                                                                   , event = Nothing
                                                                   , guard = Nothing
                                                                   , lineNr = 1
                                                                   , selected = Col.ModelData.NO }) "A->B\n"
                                 internal state = Expect.equal (PU.makeTransitionStr state
                                                                    {action = Just "action1"
                                                                    ,  endState = Nothing
                                                                    , event = Just "EI"
                                                                    , guard = Just "guard1"
                                                                    , lineNr = 1
                                                                    , selected = Col.ModelData.NO }) "A: EI [guard1] / action1\n"
                             in
                                 Expect.all [allFields,noEvent,onlyAction,anonymous,internal] "A"

                , test "Testing state strings" <|
                              \_ ->
                                  let
                                      stateA = [{ name = "A",
                                                     transitions = [
                                                      { action = Just "action1"
                                                      , endState = Just "B"
                                                      , event = Just "event1"
                                                      , guard = Just "guard1"
                                                      , lineNr = 1
                                                      , selected = Col.ModelData.NO }
                                                     ,{ action = Just "actionI"
                                                      , endState = Nothing
                                                      , event = Just "eventI"
                                                      , guard = Just "guardI"
                                                      , lineNr = 2
                                                      , selected = Col.ModelData.NO }] }
                                               ,{ name = "B",
                                                     transitions = [
                                                      { action = Just "action2"
                                                      , endState = Just "A"
                                                      , event = Just "event2"
                                                      , guard = Just "guard2"
                                                      , lineNr = 1
                                                      , selected = Col.ModelData.NO }
                                                     ,{ action = Just "actionC"
                                                      , endState = Just "C"
                                                      , event = Just "eventC"
                                                      , guard = Just "guardC"
                                                      , lineNr = 2
                                                      , selected = Col.ModelData.NO }] }

                                               ]

                                  in
                                      Expect.equal expectedStateStr <|
                                          (Debug.log "stateStr: " (PU.makeStateTranstionStr stateA))

                ,test "System plantuml Str" <|
                    \_ ->
                            Expect.equal (Debug.log "SystemStr "expectedSystemStr) <| PU.makeSystemString  <| PU.createSystem sampleData



        ]
