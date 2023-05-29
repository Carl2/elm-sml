module Col.ModelData exposing (Model,convertToStringList)
import Maybe


type alias RowData =
    { startState : Maybe String
    ,endState: Maybe String
    ,event: Maybe String
    ,guard: Maybe String
    ,action: Maybe String
    }



type alias TableDataRow = { rowIndex : Int
                          ,selected: String
                          ,data : List RowData
                          }


type alias Model =
    { tableData : List TableDataRow
    ,systemName : String
    ,mainContent : String
    }


-------------------------------------------------------------------------------
-- During conversion to this construct
-- I will create a function that creates a List (List String)
-- Where the first list represent the row, and the last has five fields of
-- data
-------------------------------------------------------------------------------
rowDataToStringList : RowData -> List String
rowDataToStringList rowData =
    [ Maybe.withDefault "" rowData.startState
    , Maybe.withDefault "" rowData.endState
    , Maybe.withDefault "" rowData.event
    , Maybe.withDefault "" rowData.guard
    , Maybe.withDefault "" rowData.action
    ]



convertToStringList: Model -> List (List String)
convertToStringList model =
    List.concatMap (\row -> List.map rowDataToStringList row.data) model.tableData
