module NewModel exposing (testPlantuml,testExtract)

import Col.ModelData exposing (..)
import Expect
import Test exposing (..)
import Col.CppData exposing(..)
import Col.PlantUml as PU

testModel : Model
testModel =
    { tableData =
          [
           { rowIndex = 1
           , selected = "selected"
           , data =
                 { startState = Just "startState1"
                 , endState = Just "endState1"
                 , event = Nothing
                 , guard = Just "guard1"
                 , action = Nothing
                 }
           }
          , { rowIndex = 2
            , selected = "selected"
            , data =
                { startState = Just "startState2"
                , endState = Nothing
                , event = Just "event2"
                , guard = Nothing
                , action = Just "action2"
                }
            }
          , { rowIndex = 3
            , selected = "selected"
            , data =
                { startState = Just "startState3"
                , endState = Just "endState3"
                , event = Nothing
                , guard = Nothing
                , action = Just "action3"
                }
            }
          , { rowIndex = 4
            , selected = "selected"
            , data =
                  { startState = Just "startState4"
                  , endState = Nothing
                  , event = Just "event4"
                  , guard = Just "guard4"
                  , action = Nothing
                  }
            }
          , { rowIndex = 5
            , selected = "selected"
            , data =
                  { startState = Just "startState5"
                  , endState = Just "endState5"
                  , event = Just "event5"
                  , guard = Nothing
                  , action = Nothing
                  }
            }
          ]
    , systemName = "Test System"
    , mainContent = "Main Content Here"
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
                         { rowIndex = 1
                          , selected = "selected"
                          , data =  rowData
                          }


                    model =
                        { tableData = [tableData]
                        , systemName = "TestSystem"
                        , mainContent = ""
                        }
                in
                    convertToStringList model
        |> Expect.equal [ [ "start", "", "event", "", "action" ] ]
        , test "Test something" <|
            \_ ->
                convertToStringList testModel
        |> Expect.equal [["startState1","endState1","","guard1",""]
                        ,["startState2","","event2","","action2"]
                        ,["startState3","endState3","","","action3"]
                        ,["startState4","","event4","guard4",""]
                        ,["startState5","endState5","event5","",""]
                        ]
           ,test "Check init" <|
              \_ ->
                  let
                      expected = {mainContent = "\nint main(int argc, char *argv[])\n{\n    sml::sm<StateMachine> sm{};\n    return EXIT_SUCCESS;\n}\n"
                                 , systemName = "StateMachine"
                                 , tableData = [{ data = { action = Nothing
                                                         , endState = Nothing
                                                         , event = Nothing
                                                         , guard = Nothing
                                                         , startState = Nothing }
                                                , rowIndex = 0, selected = "No Special" }
                                               ,{ data = { action = Nothing
                                                         , endState = Nothing
                                                         , event = Nothing
                                                         , guard = Nothing
                                                         , startState = Nothing }
                                                , rowIndex = 1
                                                , selected = "No Special" }
                                               ,{ data = { action = Nothing
                                                         , endState = Nothing
                                                         , event = Nothing
                                                         , guard = Nothing
                                                         , startState = Nothing }
                                                , rowIndex = 2
                                                , selected = "No Special" }
                                               ,{ data = { action = Nothing
                                                         , endState = Nothing
                                                         , event = Nothing
                                                         , guard = Nothing
                                                         , startState = Nothing }
                                                , rowIndex = 3
                                                , selected = "No Special" }
                                               ,{ data = { action = Nothing
                                                         , endState = Nothing
                                                         , event = Nothing
                                                         , guard = Nothing
                                                         , startState = Nothing }
                                                , rowIndex = 4
                                                , selected = "No Special" }] }
                  in
                      Expect.equal (init ()) (expected,Cmd.none)
        ,test "Check the update functio on index" <|
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

                    str = makeFsmRowFromData rowData 0 NO
                in

                    str |> Expect.equal "*startState5           +event<event5>                                  = endState5\n        "
        ,test "Test with 2" <|
            \_ ->
                let
                    mymodel ={ tableData =
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
                             , systemName = "TestSystem"
                             , mainContent = ""
                             }
                    expected = "*startState1                                [guard1]                   = endState1\n        ,startState2           +event<event2>       [guard2]            / (action2)               \n        "
                in
                    makeFsmRowFromModel mymodel |> Expect.equal expected
        ,test "Test getting all unique states" <|
            \_ ->
                let

                    myModel = {testModel | tableData = testModel.tableData ++ testModel.tableData}
                    listStates = getAllStates testModel
                in
                    listStates |> Expect.equal ["endState1","endState3","endState5","startState1","startState2","startState3","startState4","startState5"]
        ]

testPlantuml: Test
testPlantuml = describe "PlantUml using new model"
        [test "Testing the new model transformation" <|
             \_ ->
             let
                 uniqueStates = getAllStates testModel
                 newSystem = PU.genSystem "System" uniqueStates  <| PU.transformTR2Transition testModel.tableData
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
                                                    , lineNr = 1 }] }
                                  ,{ name = "startState2"
                                   , transitions = [{ action = Just "action2"
                                                    , endState = Nothing
                                                    , event = Just "event2"
                                                    , guard = Nothing
                                                    , lineNr = 2 }] }
                                  ,{ name = "startState3"
                                   , transitions = [{ action = Just "action3"
                                                    , endState = Just "endState3"
                                                    , event = Nothing
                                                    , guard = Nothing
                                                    , lineNr = 3 }] }
                                  ,{ name = "startState4"
                                   , transitions = [{ action = Nothing
                                                    , endState = Nothing
                                                    , event = Just "event4"
                                                    , guard = Just "guard4"
                                                    , lineNr = 4 }] }
                                  ,{ name = "startState5"
                                   , transitions = [{ action = Nothing
                                                    , endState = Just "endState5"
                                                    , event = Just "event5"
                                                    , guard = Nothing
                                                    , lineNr = 5 }] }]
             in

                 newSystem |> Expect.equal {name = "System", states = expectedStates}
             ]
