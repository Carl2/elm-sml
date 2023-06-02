module NewModel exposing (..)

import Col.ModelData exposing (..)
import Expect
import Test exposing (..)

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


tests : Test
tests =
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
        ]
