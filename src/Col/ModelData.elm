module Col.ModelData exposing (Model,TableDataRow,RowData,Selected(..)
                                   ,defaultRowData
                                   ,convertToStringList
                                   ,init
                                   ,rowDataToStringList
                                   ,updateDataAtIndex
                                   ,updateSelected
                                   ,convertSelected
                              )
import Maybe
import Col.Default as DF


type alias RowData =
    { startState : Maybe String
    ,endState: Maybe String
    ,event: Maybe String
    ,guard: Maybe String
    ,action: Maybe String
    }



type alias TableDataRow  = { rowIndex : Int
                          ,selected: String
                          ,data : RowData
                          }


type alias Model =
    { tableData : List TableDataRow
    ,systemName : String
    ,mainContent : String
    }

type Selected =
    NO
    | ON_ENTRY
    | ON_EXIT


convertSelected: String -> Maybe Selected
convertSelected str =
    let
        lowStr = String.toLower str
    in
        case lowStr of
            "no special" -> Just NO
            "on entry" -> Just ON_ENTRY
            "on exit" -> Just ON_EXIT
            _ -> Nothing



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
    , systemName = DF.defaultName
    , mainContent = DF.makeMain DF.defaultName
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

--
-------------------------------------------------------------------------------
--                                  Update Row data at a index
--  The index is the field index accordingly
-------------------------------------------------------------------------------
updateDataAtIndex: Int -> String -> RowData -> RowData
updateDataAtIndex index newValue rowData =
    case index of
        0 -> { rowData | startState = Just newValue }
        1 -> { rowData | endState = Just newValue }
        2 -> { rowData | event = Just newValue }
        3 -> { rowData | guard = Just newValue }
        4 -> { rowData | action = Just newValue }
        _ -> rowData


updateSelected : Model -> Int -> String -> Model
updateSelected model rowIndex newValue =
    let
        updateRow row =
            if row.rowIndex == rowIndex then
                { row | selected = newValue }
            else
                row
    in
    { model | tableData = List.map updateRow model.tableData }
