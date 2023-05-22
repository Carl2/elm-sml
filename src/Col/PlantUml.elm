module Col.PlantUml exposing (convertTable,plantUmlDataToString,PlantUmlData,uniqueStates,makeTransitionStr,createSystem
                             ,getStateByName,getStateByIndex,findStateByLineNr,makeStateTranstionStr,makeSystemString)
--import String.Interpolate exposing(interpolate)
import Set
-- import String
plantUmlUrl = "http://www.plantuml.com/plantuml/uml/~h"
--test="http://www.plantuml.com/plantuml/uml/~h407374617274756d6c0a416c6963652d3e426f62203a204920616d207573696e67206865780a40656e64756d6c"

type alias TransitionRow =
    { startState : String
    ,endState: String
    ,event: String
    ,guard: String
    ,action: String
    ,lineNr: Int
    }


type alias PlantUmlData =
    { name : String
    , transitionTable: List TransitionRow
    }

type alias Transition =
    {endState : Maybe String
    ,event: Maybe String
    ,guard: Maybe String
    ,action: Maybe String
    ,lineNr: Int
    }

type alias State =
    { name : String
      ,transitions: List Transition}

type alias System =
    { name : String
    ,states : List State
    }

isEmpty: String -> Maybe String
isEmpty str =
    case str of
        "" -> Nothing
        val -> Just val

-------------------------------------------------------------------------------
--                       Converts into plantuml struct                        --
-------------------------------------------------------------------------------
convertTable: String -> List (List String) -> PlantUmlData
convertTable smName tableList =
    {name=smName
    ,transitionTable = makeTransitionRows tableList
    }


makeTransitionRows: List (List String) -> List TransitionRow
makeTransitionRows rowList =
    List.indexedMap (\idx row -> createTransitionRow row idx) rowList

createTransitionRow : List String -> Int -> TransitionRow
createTransitionRow list idx=
    { startState = List.head list |> Maybe.withDefault ""
    , endState = List.drop 1 list |> List.head  |> Maybe.withDefault ""
    , event = List.drop 2 list |> List.head |> Maybe.withDefault ""
    , guard = List.drop 3 list |> List.head |> Maybe.withDefault ""
    , action = List.drop 4 list |> List.head  |> Maybe.withDefault ""
    , lineNr = idx
    }

getInitialStartState: PlantUmlData -> String
getInitialStartState data =
    let
        initStartState = data.transitionTable
            |> List.head
            |> Maybe.map .startState
            |> Maybe.withDefault ""
    in
        "[*] --> " ++ initStartState ++ "\n"

--TODO: Probably needs to check if event,guard and action all are empty
--Then skip the ":"
transitionRowToString : TransitionRow -> String
transitionRowToString row =
    let
        guardStr = if String.isEmpty row.guard then
                       ""
                   else
                       " [" ++ row.guard ++ "] "
        actionStr = if String.isEmpty row.action then
                        ""
                    else
                        " / " ++ row.action
        endState = case row.endState of
                       "" -> row.startState
                       "X" -> "[*]"
                       _   -> row.endState

    in
        if String.isEmpty row.startState then
           ""
        else
            row.startState ++ "->" ++ endState ++ ": " ++ row.event ++ guardStr ++ actionStr ++ "\n"

-------------------------------------------------------------------------------
--                              GetUnique states
-- iterate through all the TransitionRow items and get all the unique
-- startState and endState except for any state that is named X, the
-- unique states should be placed in a list.
-------------------------------------------------------------------------------


uniqueStates : List TransitionRow -> List String
uniqueStates transitionTable =
    let
        startStates = List.map .startState transitionTable
        endStates = List.map .endState transitionTable
        allStates = startStates ++ endStates
        filteredStates = List.filter (\state -> state /= "X" && state /= "") allStates
    in
    Set.toList (Set.fromList filteredStates)



-------------------------------------------------------------------------------
--                    Converts transitionRow to Transition                   --
--
-------------------------------------------------------------------------------
createTransitions: String -> List TransitionRow -> List Transition
createTransitions stateName rows =
    rows
        |> List.filter (\row -> row.startState == stateName )
        |> convertTransitionRowToTransition

convertTransitionRowToTransition: List TransitionRow -> List Transition
convertTransitionRowToTransition transRows =
    let
       convertToTransition row = { endState = isEmpty row.endState
                                 , event = isEmpty row.event
                                 , guard = isEmpty row.guard
                                 , action = isEmpty row.action
                                 ,lineNr = row.lineNr
                                 }
    in
        List.map convertToTransition transRows

--List.map convertToTransition transRows
createStateStructure: List TransitionRow -> List State
createStateStructure transitionTable =
    uniqueStates transitionTable |>
                                   List.map (\state -> { name = state
                                                       , transitions =
                                                             (createTransitions state transitionTable)
                                                       })


createSystem: PlantUmlData -> System
createSystem plantumlData =
    { name = plantumlData.name
    , states = createStateStructure plantumlData.transitionTable
    }

-------------------------------------------------------------------------------
--                             Generates strings                             --
-------------------------------------------------------------------------------
-- PlantUml header
headerStartStr: String
headerStartStr =
    "@startuml\n\n"

headerEndStr: String
headerEndStr =
    "@enduml\n"



systemStartStr: String -> String
systemStartStr name =
    "state " ++ name ++ "{\n"

systemEndStr: String
systemEndStr = "}\n"

-- makeAttrStr: Maybe String -> Maybe String -> Maybe String -> String
-- makeAttrStr  preStr attributeStr postStr  =
--     case mStr of
--         Nothing -> ""
--         Just str ->  preStr ++ str

guardStra: Maybe String -> Maybe String
guardStra maybeguard =
    case maybeguard of
        Nothing -> Nothing
        Just guard -> Just (" [" ++ guard ++ "]")

actionStra: Maybe String -> Maybe String
actionStra maybeAction =
    case maybeAction of
        Nothing -> Nothing
        Just action -> Just (" / " ++ action)

eventStra: Maybe String -> Maybe String
eventStra maybeEv =
    case maybeEv of
        Nothing -> Nothing
        Just event -> Just (" " ++ event)

systemAttributeStr: Transition -> String
systemAttributeStr tr =
    let
        ev = Maybe.withDefault ""  (eventStra tr.event)
        guard = Maybe.withDefault "" (guardStra tr.guard)
        action = Maybe.withDefault "" (actionStra tr.action)
    in
    case (ev,guard,action) of
        ("","","") -> ""
        (_,_,_) -> ":" ++ ev ++ guard ++ action



makeTransitionStr: String -> Transition -> String
makeTransitionStr name tr =
    case tr.endState of
        Nothing -> name ++ systemAttributeStr tr ++ "\n"
        Just endState -> name ++ "->" ++ endState ++ systemAttributeStr tr ++ "\n"


makeStateTranstionStr: List State -> String
makeStateTranstionStr states =
        List.foldl (\state acc ->
                        let
                            lstTransStr = List.map (\trans -> makeTransitionStr state.name trans) state.transitions
                        in
                            acc ++ (String.concat lstTransStr)
                        ) "" states

makeSystemString: System -> String
makeSystemString system =
    headerStartStr ++ (systemStartStr system.name) ++
        (makeStateTranstionStr system.states) ++ systemEndStr ++ headerEndStr





-- stateStr: State -> String<
-- stateStr state =
--     let
--         List.map (\transition -> makeTransitionStr state.name transition)
--     in
--         state.name ++


-- generatePlantUmlString: System -> Maybe String
-- generatePlantUmlString system =
--         "@startuml\n\n" ++ createNameStr system.name





-- getTransitions: String -> List TransitionRow -> List Transition
-- getTransitions state transitions =
--     List.map (\trans ->
--                   {
--                   ,endState = isEmpty trans.endState
--                   ,event = isEmpty trans.event
--                   ,guard = isEmpty trans.guard
--                   ,action= isEmpty trans.action
--                   }) transitions


-- parseTransition: Transition -> String
-- parseTransition trans =
--     case trans.

-- parseTransitions: List Transition -> String
-- parseTransitions transes =


plantUmlDataToString : PlantUmlData -> String
plantUmlDataToString data =
    "@startuml\n\n" ++ getInitialStartState data
        ++ (data.transitionTable
           |> List.map transitionRowToString
           |> String.concat
           )
        ++ "@enduml\n"


-------------------------------------------------------------------------------
--                        Useful functions for system                        --
-------------------------------------------------------------------------------
getStateByIndex: Int -> System -> Maybe State
getStateByIndex index system=
    system.states
        |> List.drop index
        |> List.head

getStateByName: String -> System -> Maybe State
getStateByName name system =
    let
        lstState = List.filter (\state -> state.name == name) system.states
    in
        case lstState of
            [state] -> Just state
            [] -> Nothing
            _ -> Debug.log "Something is strange" Nothing



findStateByLineNr :  Int -> System -> Maybe State
findStateByLineNr lineNr system  =
    List.foldl
        (\state acc ->
             case acc of
                 Just _ ->
                     acc

                 Nothing ->
                     if List.any (\transition -> transition.lineNr == lineNr) state.transitions then
                         Just state
                     else
                         Nothing
        )
        Nothing
        system.states
