module Col.ModelData exposing (Model,Machine,TableDataRow,RowData,Selected(..)
                                     ,LogFormat(..),SmPolicy(..)
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
                                     ,encodeModel
                                     ,modelDecoder
                                )
import Maybe
import Set
import Col.Default as DF
import Json.Encode as E
import Json.Decode as D

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

type LogFormat
    = Printf
    | StdPrint
    | EmptyLog


type SmPolicy
    = Logger LogFormat
    | DeferQueue String
    | ThreadSafe String


type alias Model =
    { machines : List Machine
    , activeMachine : Int
    , mainContent : String
    , contextTypes : List String
    , smPolicies : List SmPolicy
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
     , mainContent = DF.makeMain DF.defaultName [] "" ""
     , contextTypes = []
     , smPolicies = []
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


-------------------------------------------------------------------------------
-- JSON Encoder
-------------------------------------------------------------------------------

encodeMaybe : (a -> E.Value) -> Maybe a -> E.Value
encodeMaybe encoder maybe =
    case maybe of
        Just val -> encoder val
        Nothing -> E.null


encodeRowData : RowData -> E.Value
encodeRowData rd =
    E.object
        [ ("startState", encodeMaybe E.string rd.startState)
        , ("endState", encodeMaybe E.string rd.endState)
        , ("event", encodeMaybe E.string rd.event)
        , ("guard", encodeMaybe E.string rd.guard)
        , ("action", encodeMaybe E.string rd.action)
        ]


encodeTableDataRow : TableDataRow -> E.Value
encodeTableDataRow row =
    E.object
        [ ("rowIndex", E.int row.rowIndex)
        , ("selected", E.string row.selected)
        , ("data", encodeRowData row.data)
        ]


encodeMachine : Machine -> E.Value
encodeMachine machine =
    E.object
        [ ("name", E.string machine.name)
        , ("tableData", E.list encodeTableDataRow machine.tableData)
        ]


encodeModel : Model -> E.Value
encodeModel model =
    E.object
        [ ("machines", E.list encodeMachine model.machines)
        , ("activeMachine", E.int model.activeMachine)
        , ("mainContent", E.string model.mainContent)
        , ("contextTypes", E.list E.string model.contextTypes)
        , ("smPolicies", E.list encodeSmPolicy model.smPolicies)
        ]


encodeLogFormat : LogFormat -> E.Value
encodeLogFormat fmt =
    case fmt of
        Printf -> E.string "printf"
        StdPrint -> E.string "std_print"
        EmptyLog -> E.string "empty"


encodeSmPolicy : SmPolicy -> E.Value
encodeSmPolicy policy =
    case policy of
        Logger fmt ->
            E.object [("type", E.string "logger"), ("format", encodeLogFormat fmt)]
        DeferQueue queueType ->
            E.object [("type", E.string "defer_queue"), ("queueType", E.string queueType)]
        ThreadSafe mutexType ->
            E.object [("type", E.string "thread_safe"), ("mutexType", E.string mutexType)]


-------------------------------------------------------------------------------
-- JSON Decoder
-------------------------------------------------------------------------------

rowDataDecoder : D.Decoder RowData
rowDataDecoder =
    D.map5 RowData
        (D.field "startState" (D.nullable D.string))
        (D.field "endState" (D.nullable D.string))
        (D.field "event" (D.nullable D.string))
        (D.field "guard" (D.nullable D.string))
        (D.field "action" (D.nullable D.string))


tableDataRowDecoder : D.Decoder TableDataRow
tableDataRowDecoder =
    D.map3 TableDataRow
        (D.field "rowIndex" D.int)
        (D.field "selected" D.string)
        (D.field "data" rowDataDecoder)


machineDecoder : D.Decoder Machine
machineDecoder =
    D.map2 Machine
        (D.field "name" D.string)
        (D.field "tableData" (D.list tableDataRowDecoder))


modelDecoder : D.Decoder Model
modelDecoder =
    D.map5 Model
        (D.field "machines" (D.list machineDecoder))
        (D.field "activeMachine" D.int)
        (D.field "mainContent" D.string)
        (D.field "contextTypes" (D.list D.string))
        (D.oneOf
            [ D.field "smPolicies" (D.list smPolicyDecoder)
            , D.succeed []
            ])


logFormatDecoder : D.Decoder LogFormat
logFormatDecoder =
    D.string |> D.andThen (\s ->
        case s of
            "printf" -> D.succeed Printf
            "std_print" -> D.succeed StdPrint
            "empty" -> D.succeed EmptyLog
            _ -> D.fail ("Unknown log format: " ++ s)
    )


smPolicyDecoder : D.Decoder SmPolicy
smPolicyDecoder =
    D.field "type" D.string |> D.andThen (\t ->
        case t of
            "logger" -> D.map Logger (D.field "format" logFormatDecoder)
            "defer_queue" -> D.map DeferQueue (D.field "queueType" D.string)
            "thread_safe" -> D.map ThreadSafe (D.field "mutexType" D.string)
            _ -> D.fail ("Unknown policy type: " ++ t)
    )
