module Col.ModelData exposing (Model,TableDataRow,RowData,convertToStringList,init)
import Maybe
import Col.CppData as Cpp


type alias RowData =
    { startState : Maybe String
    ,endState: Maybe String
    ,event: Maybe String
    ,guard: Maybe String
    ,action: Maybe String
    }



type alias TableDataRow = { rowIndex : Int
                          ,selected: String
                          ,data : RowData
                          }


type alias Model =
    { tableData : List TableDataRow
    ,systemName : String
    ,mainContent : String
    }


defaultRowData : RowData
defaultRowData =
    { startState = Nothing
    , endState = Nothing
    , event = Nothing
    , guard = Nothing
    , action = Nothing
    }

createTableDataRow : Int -> TableDataRow
createTableDataRow index =
    { rowIndex = index
    , selected = "No Special"
    , data = defaultRowData
    }


init: () -> (Model, Cmd msg)
init _ =
    ({ tableData = List.map createTableDataRow <| List.range 0 4
    , systemName = Cpp.defaultName
    , mainContent = Cpp.makeMain Cpp.defaultName
    },Cmd.none)


-------------------------------------------------------------------------------
-- During conversion to this construct
-- I will create a function that creates a List (List String)
-- Where the first list represent the row, and the last has five fields of
-- data
-------------------------------------------------------------------------------
rowDataToStringList : RowData -> List String
rowDataToStringList rowData =
    [Maybe.withDefault "" rowData.startState
    , Maybe.withDefault "" rowData.endState
    , Maybe.withDefault "" rowData.event
    , Maybe.withDefault "" rowData.guard
    , Maybe.withDefault "" rowData.action
    ]






convertToStringList: Model -> List (List String)
convertToStringList model =
    List.map (\rowData -> rowDataToStringList rowData.data) model.tableData
