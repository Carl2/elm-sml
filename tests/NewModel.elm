module NewModel exposing (..)

import Col.ModelData exposing (..)
import Expect
import Test exposing (..)

testModel : Model
testModel =
    { tableData =
        [ { rowIndex = 1
          , selected = "selected"
          , data =
              [ { startState = Just "startState1"
                , endState = Just "endState1"
                , event = Nothing
                , guard = Just "guard1"
                , action = Nothing
                }
              ]
          }
        , { rowIndex = 2
          , selected = "selected"
          , data =
              [ { startState = Just "startState2"
                , endState = Nothing
                , event = Just "event2"
                , guard = Nothing
                , action = Just "action2"
                }
              ]
          }
        , { rowIndex = 3
          , selected = "selected"
          , data =
              [ { startState = Just "startState3"
                , endState = Just "endState3"
                , event = Nothing
                , guard = Nothing
                , action = Just "action3"
                }
              ]
          }
        , { rowIndex = 4
          , selected = "selected"
          , data =
              [ { startState = Just "startState4"
                , endState = Nothing
                , event = Just "event4"
                , guard = Just "guard4"
                , action = Nothing
                }
              ]
          }
        , { rowIndex = 5
          , selected = "selected"
          , data =
              [ { startState = Just "startState5"
                , endState = Just "endState5"
                , event = Just "event5"
                , guard = Nothing
                , action = Nothing
                }
              ]
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
                        [ { rowIndex = 1
                          , selected = "selected"
                          , data = [ rowData ]
                          }
                        ]

                    model =
                        { tableData = tableData
                        , systemName = "TestSystem"
                        , mainContent = ""
                        }
                in
                convertToStringList model
                    |> Expect.equal [ [ "start", "", "event", "", "action" ] ]
        ]
