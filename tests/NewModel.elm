module NewModel exposing (testPlantuml,testExtract,testSubSm,testContextInjection,testRowOperations,testLocalStorage)

import Col.ModelData exposing (..)
import Expect
import Test exposing (..)
import Col.CppData exposing(..)
import Col.PlantUml as PU
import Col.Default as DF
import Main exposing (update, Msg(..))
import Json.Decode as D
import Json.Encode as E


testMachine : Machine
testMachine =
    { name = "TestMachine"
    , tableData =
          [
           { rowIndex = 0
           , selected = "No Special"
           , data =
                 { startState = Just "startState1"
                 , endState = Just "endState1"
                 , event = Nothing
                 , guard = Just "guard1"
                 , action = Nothing
                 }
           }
          , { rowIndex = 1
            , selected = "No Special"
            , data =
                { startState = Just "startState2"
                , endState = Nothing
                , event = Just "event2"
                , guard = Nothing
                , action = Just "action2"
                }
            }
          , { rowIndex = 2
            , selected = "No Special"
            , data =
                { startState = Just "startState3"
                , endState = Just "endState3"
                , event = Nothing
                , guard = Nothing
                , action = Just "action3"
                }
            }
          , { rowIndex = 3
            , selected = "No Special"
            , data =
                  { startState = Just "startState4"
                  , endState = Nothing
                  , event = Just "event4"
                  , guard = Just "guard4"
                  , action = Nothing
                  }
            }
          , { rowIndex = 4
            , selected = "No Special"
            , data =
                  { startState = Just "startState5"
                  , endState = Just "endState5"
                  , event = Just "event5"
                  , guard = Nothing
                  , action = Nothing
                  }
            }
          ]
    }


testModel : Model
testModel =
    { machines = [testMachine]
    , activeMachine = 0
    , mainContent = "Main Content Here"
    , contextTypes = []
    }


testExtract : Test
testExtract =
    describe "extractRowData"
        [ test "it should replace Nothing with an empty string" <|
            \_ ->
                let
                    rowData =
                        { startState = Just "start"
                        , endState = Nothing
                        , event = Just "event"
                        , guard = Nothing
                        , action = Just "action"
                        }

                    tableData =
                         { rowIndex = 0
                          , selected = "No Special"
                          , data =  rowData
                          }

                    machine =
                        { name = "TestSystem"
                        , tableData = [tableData]
                        }
                in
                    convertToStringList machine
        |> Expect.equal [ [ "start", "", "event", "", "action" ] ]
        , test "Test something" <|
            \_ ->
                convertToStringList testMachine
        |> Expect.equal [["startState1","endState1","","guard1",""]
                        ,["startState2","","event2","","action2"]
                        ,["startState3","endState3","","","action3"]
                        ,["startState4","","event4","guard4",""]
                        ,["startState5","endState5","event5","",""]
                        ]
           ,test "Check init" <|
              \_ ->
                  let
                      (initModel, _) = init ()
                      rootName = getRootName initModel
                  in
                      Expect.equal rootName "StateMachine"
        ,test "Check init has one machine with 5 rows" <|
              \_ ->
                  let
                      (initModel, _) = init ()
                      machineCount = List.length initModel.machines
                      rowCount = case getActiveMachine initModel of
                                     Just m -> List.length m.tableData
                                     Nothing -> 0
                  in
                      Expect.all
                          [ \_ -> Expect.equal machineCount 1
                          , \_ -> Expect.equal rowCount 5
                          ] ()
        ,test "Check the update function on index" <|
            \_ ->
                let
                    rowData = { action = Nothing
                              , endState = Nothing
                              , event = Nothing
                              , guard = Nothing
                              , startState = Nothing }

                    rowDataExpected = {action = Nothing
                                      , endState = Nothing
                                      , event = Just "event"
                                      , guard = Nothing
                                      , startState = Just "state1" }

                    val = updateDataAtIndex 2 "event" rowData
                          |> updateDataAtIndex 0 "state1"

                in
                    Expect.equal rowDataExpected val
        ,test "Test makeFsmRowFromData" <|
            \_ ->
                let
                    rowData ={ startState = Just "startState5"
                             , endState = Just "endState5"
                             , event = Just "event5"
                             , guard = Nothing
                             , action = Nothing
                             }

                    str = makeFsmRowFromData [] rowData 0 NO
                in

                    str |> Expect.equal "*startState5           +event<event5>                                  = endState5\n        "
        ,test "Test makeFsmRowFromMachine" <|
            \_ ->
                let
                    myMachine = { name = "TestSystem"
                               , tableData =
                                   [
                                    { rowIndex = 0
                                    , selected = "No Special"
                                    , data =
                                          { startState = Just "startState1"
                                          , endState = Just "endState1"
                                          , event = Nothing
                                          , guard = Just "guard1"
                                          , action = Nothing
                                          }
                                    }
                                   , { rowIndex = 1
                                     , selected = "No Special"
                                     , data =
                                           { startState = Just "startState2"
                                           , endState = Nothing
                                           , event = Just "event2"
                                           , guard = Just "guard2"
                                           , action = Just "action2"
                                           }
                                     }
                                   ]
                               }
                    expected = "*startState1                                [guard1]                   = endState1\n        ,startState2           +event<event2>       [guard2]            / (action2)               \n        "
                in
                    makeFsmRowFromMachine [] myMachine |> Expect.equal expected
        ,test "Test getting all unique states" <|
            \_ ->
                let
                    listStates = getAllStates testMachine
                in
                    listStates |> Expect.equal ["endState1","endState3","endState5","startState1","startState2","startState3","startState4","startState5"]
        ,test "Internal transition: no end state in C++ output" <|
            \_ ->
                let
                    rowData = { startState = Just "Idle"
                              , endState = Nothing
                              , event = Just "tick"
                              , guard = Nothing
                              , action = Just "doStuff"
                              }
                    result = makeFsmRowFromData [] rowData 1 NO
                in
                -- Should NOT contain "= " (no target state assignment)
                Expect.all
                    [ \r -> Expect.equal True (String.contains "+event<tick>" r)
                    , \r -> Expect.equal True (String.contains "/ (doStuff)" r)
                    , \r -> Expect.equal False (String.contains "= " r)
                    ] result
        ]


testPlantuml: Test
testPlantuml = describe "PlantUml using new model"
        [test "Testing the new model transformation" <|
             \_ ->
             let
                 uniqueStates = getAllStates testMachine
                 newSystem = PU.genSystem "System" uniqueStates  <| PU.transformTR2Transition testMachine.tableData
                 expectedStates = [{ name = "endState1"
                                   , transitions = [] }
                                  ,{ name = "endState3"
                                   , transitions = [] }
                                  ,{ name = "endState5"
                                   , transitions = [] }
                                  ,{ name = "startState1"
                                   , transitions = [{ action = Nothing
                                                    , endState = Just "endState1"
                                                    , event = Nothing
                                                    , guard = Just "guard1"
                                                    , lineNr = 0
                                                    , selected = NO }] }
                                  ,{ name = "startState2"
                                   , transitions = [{ action = Just "action2"
                                                    , endState = Nothing
                                                    , event = Just "event2"
                                                    , guard = Nothing
                                                    , lineNr = 1
                                                    , selected = NO }] }
                                  ,{ name = "startState3"
                                   , transitions = [{ action = Just "action3"
                                                    , endState = Just "endState3"
                                                    , event = Nothing
                                                    , guard = Nothing
                                                    , lineNr = 2
                                                    , selected = NO }] }
                                  ,{ name = "startState4"
                                   , transitions = [{ action = Nothing
                                                    , endState = Nothing
                                                    , event = Just "event4"
                                                    , guard = Just "guard4"
                                                    , lineNr = 3
                                                    , selected = NO }] }
                                  ,{ name = "startState5"
                                   , transitions = [{ action = Nothing
                                                    , endState = Just "endState5"
                                                    , event = Just "event5"
                                                    , guard = Nothing
                                                    , lineNr = 4
                                                    , selected = NO }] }]
             in

                 newSystem |> Expect.equal {name = "System", states = expectedStates}
             ]


-------------------------------------------------------------------------------
--                         Sub-SM Tests                                      --
-------------------------------------------------------------------------------

testSubSm : Test
testSubSm =
    describe "Sub-state machine tests"
        [ test "isSubSM detects machine name as sub-SM" <|
            \_ ->
                isSubSM ["ChildMachine", "OtherMachine"] "ChildMachine"
                    |> Expect.equal True
        , test "isSubSM returns False for regular state" <|
            \_ ->
                isSubSM ["ChildMachine", "OtherMachine"] "SomeState"
                    |> Expect.equal False
        , test "generateAllStructs produces leaf-first ordering" <|
            \_ ->
                let
                    childMachine =
                        { name = "ChildSM"
                        , tableData =
                            [ { rowIndex = 0, selected = "No Special"
                              , data = { startState = Just "C1", endState = Just "C2", event = Just "ev1", guard = Nothing, action = Nothing }
                              }
                            ]
                        }
                    parentMachine =
                        { name = "ParentSM"
                        , tableData =
                            [ { rowIndex = 0, selected = "No Special"
                              , data = { startState = Just "Idle", endState = Just "ChildSM", event = Just "start", guard = Nothing, action = Nothing }
                              }
                            ]
                        }
                    result = generateAllStructs [parentMachine, childMachine]
                in
                -- ChildSM struct should appear before ParentSM struct
                Expect.equal True
                    (let
                        childPos = String.indexes "struct ChildSM" result |> List.head |> Maybe.withDefault 999
                        parentPos = String.indexes "struct ParentSM" result |> List.head |> Maybe.withDefault 0
                     in
                        childPos < parentPos
                    )
        , test "makeConstexprClass skips sub-SM states (no alias needed)" <|
            \_ ->
                let
                    machineNames = ["ChildSM"]
                    data = [["Idle", "ChildSM", "start", "", ""]]
                    result = makeConstexprClass machineNames data
                in
                Expect.all
                    [ \r -> Expect.equal False (String.contains "ChildSM" r)
                    , \r -> Expect.equal True (String.contains "sml::state<class Idle>" r)
                    ] result
        , test "Nested PlantUML generates composite state block" <|
            \_ ->
                let
                    childMachine =
                        { name = "ChildSM"
                        , tableData =
                            [ { rowIndex = 0, selected = "No Special"
                              , data = { startState = Just "C1", endState = Just "C2", event = Just "ev1", guard = Nothing, action = Nothing }
                              }
                            ]
                        }
                    parentMachine =
                        { name = "ParentSM"
                        , tableData =
                            [ { rowIndex = 0, selected = "No Special"
                              , data = { startState = Just "Idle", endState = Just "ChildSM", event = Just "start", guard = Nothing, action = Nothing }
                              }
                            ]
                        }
                    result = PU.makeNestedSystemString [parentMachine, childMachine]
                in
                Expect.all
                    [ \r -> Expect.equal True (String.contains "state ChildSM {" r)
                    , \r -> Expect.equal True (String.contains "[*]-->ChildSM_C1" r)
                    , \r -> Expect.equal True (String.contains "ChildSM_C1->ChildSM_C2: ev1" r)
                    , \r -> Expect.equal True (String.contains "state \"C1\" as ChildSM_C1" r)
                    , \r -> Expect.equal True (String.contains "@startuml" r)
                    , \r -> Expect.equal True (String.contains "@enduml" r)
                    ] result

        , test "Sub-SM referenced by both root and child is rendered only at root level" <|
            \_ ->
                let
                    -- SubA is referenced by both Root (B→SubA) and Sub (C→SubA)
                    subA =
                        { name = "SubA"
                        , tableData =
                            [ { rowIndex = 0, selected = "No Special"
                              , data = { startState = Just "X1", endState = Just "X2", event = Just "e1", guard = Nothing, action = Nothing }
                              }
                            ]
                        }
                    sub =
                        { name = "Sub"
                        , tableData =
                            [ { rowIndex = 0, selected = "No Special"
                              , data = { startState = Just "A", endState = Just "B", event = Just "e1", guard = Nothing, action = Nothing }
                              }
                            , { rowIndex = 1, selected = "No Special"
                              , data = { startState = Just "B", endState = Just "SubA", event = Just "e2", guard = Nothing, action = Nothing }
                              }
                            ]
                        }
                    root =
                        { name = "Root"
                        , tableData =
                            [ { rowIndex = 0, selected = "No Special"
                              , data = { startState = Just "Idle", endState = Just "Sub", event = Just "go", guard = Nothing, action = Nothing }
                              }
                            , { rowIndex = 1, selected = "No Special"
                              , data = { startState = Just "Idle", endState = Just "SubA", event = Just "goA", guard = Nothing, action = Nothing }
                              }
                            ]
                        }
                    result = PU.makeNestedSystemString [root, sub, subA]

                    -- Count occurrences of "state SubA {" — should be exactly 1
                    countSubA = result
                        |> String.split "state SubA {"
                        |> List.length
                        |> (\n -> n - 1)  -- split produces N+1 parts for N occurrences
                in
                Expect.all
                    [ \_ -> Expect.equal 1 countSubA  -- SubA rendered only once
                    , \r -> Expect.equal True (String.contains "state SubA {" r)
                    , \r -> Expect.equal True (String.contains "state Sub {" r)
                    -- SubA should NOT appear inside Sub
                    , \r -> Expect.equal False (String.contains "state Sub {\n  state SubA {" (String.replace "\n" "\n" r))
                    ] result
        ]


-------------------------------------------------------------------------------
--                      Context Injection Tests                              --
-------------------------------------------------------------------------------

testContextInjection : Test
testContextInjection =
    describe "Context injection tests"
        [ test "makeContextParams with no contexts returns empty string" <|
            \_ ->
                makeContextParams [] |> Expect.equal ""
        , test "makeContextParams with one context" <|
            \_ ->
                makeContextParams ["MyCtx"] |> Expect.equal ", MyCtx& ctx_"
        , test "makeContextParams with two contexts" <|
            \_ ->
                makeContextParams ["Ctx1", "Ctx2"] |> Expect.equal ", Ctx1& ctx_0, Ctx2& ctx_1"
        , test "makeContextParams with three contexts" <|
            \_ ->
                makeContextParams ["A", "B", "C"] |> Expect.equal ", A& ctx_0, B& ctx_1, C& ctx_2"
        , test "makeEventHeader with no context - guard unchanged" <|
            \_ ->
                let
                    data = [["s0", "s1", "ev", "myGuard", "myAction"]]
                    result = makeEventHeader [] data
                in
                Expect.all
                    [ \r -> Expect.equal True (String.contains "(const auto& event)" r)
                    , \r -> Expect.equal False (String.contains "ctx" r)
                    ] result
        , test "makeEventHeader with one context - guard has context param" <|
            \_ ->
                let
                    data = [["s0", "s1", "ev", "myGuard", "myAction"]]
                    result = makeEventHeader ["MyCtx"] data
                in
                Expect.all
                    [ \r -> Expect.equal True (String.contains "(const auto& event, MyCtx& ctx_)" r)
                    , \r -> Expect.equal True (String.contains "return true" r)
                    ] result
        , test "makeEventHeader with two contexts - action has both params" <|
            \_ ->
                let
                    data = [["s0", "s1", "ev", "", "doStuff"]]
                    result = makeEventHeader ["Ctx1", "Ctx2"] data
                in
                Expect.equal True (String.contains "Ctx1& ctx_0, Ctx2& ctx_1" result)
        , test "makeMain with no contexts" <|
            \_ ->
                let
                    result = DF.makeMain "SM" []
                in
                Expect.all
                    [ \r -> Expect.equal True (String.contains "sml::sm<SM> sm{};" r)
                    , \r -> Expect.equal False (String.contains "ctx" r)
                    ] result
        , test "makeMain with one context" <|
            \_ ->
                let
                    result = DF.makeMain "SM" ["MyCtx"]
                in
                Expect.all
                    [ \r -> Expect.equal True (String.contains "MyCtx ctx_{};" r)
                    , \r -> Expect.equal True (String.contains "sml::sm<SM> sm{ctx_};" r)
                    ] result
        , test "makeMain with two contexts" <|
            \_ ->
                let
                    result = DF.makeMain "SM" ["Ctx1", "Ctx2"]
                in
                Expect.all
                    [ \r -> Expect.equal True (String.contains "Ctx1 ctx_0{};" r)
                    , \r -> Expect.equal True (String.contains "Ctx2 ctx_1{};" r)
                     , \r -> Expect.equal True (String.contains "sml::sm<SM> sm{ctx_0, ctx_1};" r)
                    ] result
        ]


-------------------------------------------------------------------------------
--                         Row Operations Tests                              --
-------------------------------------------------------------------------------

testRowOperations : Test
testRowOperations =
    let
        makeRow idx startSt =
            { rowIndex = idx
            , selected = "No Special"
            , data =
                { startState = Just startSt
                , endState = Just "B"
                , event = Just "ev"
                , guard = Nothing
                , action = Nothing
                }
            }

        threeRowMachine =
            { name = "TestSM"
            , tableData = [ makeRow 0 "A", makeRow 1 "B", makeRow 2 "C" ]
            }

        baseModel =
            { machines = [ threeRowMachine ]
            , activeMachine = 0
            , mainContent = ""
            , contextTypes = []
            }

        getStartStates model =
            case List.head model.machines of
                Just m -> List.filterMap (\r -> r.data.startState) m.tableData
                Nothing -> []

        getRowIndices model =
            case List.head model.machines of
                Just m -> List.map .rowIndex m.tableData
                Nothing -> []
    in
    describe "Row operations"
        [ test "DeleteRow removes middle row" <|
            \_ ->
                let
                    (newModel, _) = update (DeleteRow 1) baseModel
                in
                Expect.equal ["A", "C"] (getStartStates newModel)

        , test "DeleteRow renumbers remaining rows" <|
            \_ ->
                let
                    (newModel, _) = update (DeleteRow 1) baseModel
                in
                Expect.equal [0, 1] (getRowIndices newModel)

        , test "MoveRowDown swaps row 0 with row 1" <|
            \_ ->
                let
                    (newModel, _) = update (MoveRowDown 0) baseModel
                in
                Expect.equal ["B", "A", "C"] (getStartStates newModel)

        , test "MoveRowUp swaps row 2 with row 1" <|
            \_ ->
                let
                    (newModel, _) = update (MoveRowUp 2) baseModel
                in
                Expect.equal ["A", "C", "B"] (getStartStates newModel)

        , test "MoveRowUp on row 0 is a no-op" <|
            \_ ->
                let
                    (newModel, _) = update (MoveRowUp 0) baseModel
                in
                Expect.equal ["A", "B", "C"] (getStartStates newModel)

        , test "MoveRowDown on last row is a no-op" <|
            \_ ->
                let
                    (newModel, _) = update (MoveRowDown 2) baseModel
                in
                Expect.equal ["A", "B", "C"] (getStartStates newModel)
        ]


testLocalStorage : Test
testLocalStorage =
    describe "localStorage encode/decode"
        [ test "round-trip: encode then decode restores model" <|
            \_ ->
                let
                    model =
                        { machines =
                            [ { name = "Root"
                              , tableData =
                                    [ { rowIndex = 0
                                      , selected = "No Special"
                                      , data =
                                            { startState = Just "Idle"
                                            , endState = Just "Running"
                                            , event = Just "start"
                                            , guard = Nothing
                                            , action = Just "doStart"
                                            }
                                      }
                                    , { rowIndex = 1
                                      , selected = "On Entry"
                                      , data =
                                            { startState = Just "Running"
                                            , endState = Nothing
                                            , event = Just "tick"
                                            , guard = Just "isReady"
                                            , action = Nothing
                                            }
                                      }
                                    ]
                              }
                            , { name = "ChildSM"
                              , tableData =
                                    [ { rowIndex = 0
                                      , selected = "No Special"
                                      , data =
                                            { startState = Just "C1"
                                            , endState = Just "C2"
                                            , event = Just "ev1"
                                            , guard = Nothing
                                            , action = Nothing
                                            }
                                      }
                                    ]
                              }
                            ]
                        , activeMachine = 1
                        , mainContent = "int main() {}"
                        , contextTypes = ["MyCtx", "Logger"]
                        }
                    encoded = E.encode 0 (encodeModel model)
                    decoded = D.decodeString modelDecoder encoded
                in
                Expect.equal (Ok model) decoded

        , test "decode failure returns Err" <|
            \_ ->
                let
                    badJson = "{\"garbage\": true}"
                    decoded = D.decodeString modelDecoder badJson
                in
                Expect.err decoded

        , test "decode null returns Err" <|
            \_ ->
                let
                    decoded = D.decodeString modelDecoder "null"
                in
                Expect.err decoded
        ]
