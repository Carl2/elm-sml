module Col.ModelData exposing (Model,Machine,TableDataRow,RowData,Selected(..)
                                   ,defaultRowData
                                   ,convertToStringList
                                   ,init
                                   ,rowDataToStringList
                                   ,updateDataAtIndex
                                   ,updateSelected
                                   ,convertSelected
                                   ,getAllStates
                                   ,getActiveMachine
                                   ,getMachineNames
                                   ,getRootName
                                   ,updateActiveMachineTableData
                              )
import Maybe
import Set
import Col.Default as DF

invalidStateNames = ["x", "X", ""]

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

type alias Machine =
    { name : String
    , tableData : List TableDataRow
    }

type alias Model =
    { machines : List Machine
    , activeMachine : Int
    , mainContent : String
    , contextTypes : List String
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
    let
        defaultMachine =
            { name = DF.defaultName
            , tableData = List.map createTableDataRow <| List.range 0 4
            }
    in
    ({ machines = [defaultMachine]
     , activeMachine = 0
     , mainContent = DF.makeMain DF.defaultName []
     , contextTypes = []
    },Cmd.none)


-------------------------------------------------------------------------------
-- Helper functions for accessing machines
-------------------------------------------------------------------------------

getActiveMachine : Model -> Maybe Machine
getActiveMachine model =
    List.drop model.activeMachine model.machines
        |> List.head


getMachineNames : Model -> List String
getMachineNames model =
    List.map .name model.machines


getRootName : Model -> String
getRootName model =
    List.head model.machines
        |> Maybe.map .name
        |> Maybe.withDefault DF.defaultName


updateActiveMachineTableData : Model -> (List TableDataRow -> List TableDataRow) -> Model
updateActiveMachineTableData model fn =
    let
        updateAt idx machines =
            List.indexedMap
                (\i machine ->
                    if i == idx then
                        { machine | tableData = fn machine.tableData }
                    else
                        machine
                )
                machines
    in
    { model | machines = updateAt model.activeMachine model.machines }


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


convertToStringList: Machine -> List (List String)
convertToStringList machine =
    List.map (\rowData -> rowDataToStringList rowData.data) machine.tableData

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

        updateMachine machine =
            { machine | tableData = List.map updateRow machine.tableData }
    in
    updateActiveMachineTableData model (\tableData -> List.map updateRow tableData)

--
-------------------------------------------------------------------------------
--             Get All state names (unique)
-------------------------------------------------------------------------------
filterOutInvalids: String -> List String -> Bool
filterOutInvalids state lstInvalids =
    not <| List.member state lstInvalids


getAllStates: Machine -> List String
getAllStates machine =
    let
        allRows = List.map .data machine.tableData
        maybeStates = (List.map .startState allRows) ++ (List.map .endState allRows)
        onlyValidStates states = List.filter
                                 (\maybeState -> case maybeState of
                                                     Nothing -> False
                                                     Just state -> filterOutInvalids state invalidStateNames) states
        validStrings states = List.map (\state -> Maybe.withDefault "" state) <| onlyValidStates states
    in
        Set.toList <| Set.fromList  <|validStrings maybeStates
