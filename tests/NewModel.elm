module NewModel exposing (newModelTest)
import Main exposing (convertTableData, Model, Msg(..))
exampleModel : Model
exampleModel =
    { tableData =
        [ { rowIndex = 1, selected = "Yes", data = ["State1", "State2", "Ev1", "Guard1", "Action1"] }
        , { rowIndex = 2, selected = "No", data = ["State2", "state3", "Ev2", "Guard2", "Action2"] }
        , { rowIndex = 3, selected = "Yes", data = ["State3", "state4", "Ev3", "Guard3", "Action3"] }
        , { rowIndex = 4, selected = "No", data = ["State4", "state5", "Ev4", "Guard4", "Action4"] }
        , { rowIndex = 5, selected = "Yes", data = ["State5", "state5", "Ev5", "Guard5", "Action5"] }
        ]
    , systemName = "Elm System"
    , mainContent = "Main content here"
    }


newModelTest: Test
newModelTest
    describe "New model test"
        [test "Convert to data to string" <|
             \_ ->
             convertTableData exampleModel.tableData |> Expect.equal [["State1", "State2", "Ev1", "Guard1", "Action1"]
                                                                     ,["State2", "state3", "Ev2", "Guard2", "Action2"]
                                                                     ,["State3", "state4", "Ev3", "Guard3", "Action3"]
                                                                     ,["State4", "state5", "Ev4", "Guard4", "Action4"]
                                                                     ,["State5", "state5", "Ev5", "Guard5", "Action5"]
                                                                     ]
             ]
        ]
