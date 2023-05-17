module Col.PlantUml exposing (convertTable,plantUmlDataToString,PlantUmlData,uniqueStates,getTransitions)
--import String.Interpolate exposing(interpolate)
import Set
-- import String
plantUmlUrl = "http://www.plantuml.com/plantuml/uml/~h"
test="http://www.plantuml.com/plantuml/uml/~h407374617274756d6c0a416c6963652d3e426f62203a204920616d207573696e67206865780a40656e64756d6c"

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
    }

type alias State =
    { name : String
      ,transitions: List Transition}

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



isEmpty: String -> Maybe String
isEmpty str =
    case str of
        "" -> Nothing
        val -> Just val

createTransitions: String -> TransitionRow -> Transition
createTransitions state rows =
    let
        -- For each of the rows find the transition with
        -- the startName == state
        allStateRows = List.filter (\row -> row.startState == state ) rows

    in

convertTransitionRowToState: String -> List TransitionRow -> State
convertTransitionRowToState stateName transRows =
    let
       convertToTransition row = { endState = isEmpty row.endState
                                 , event = isEmpty row.event
                                 , guard = isEmpty row.guard
                                 , action = isEmpty row.action}
       transes = List.map convertToTransition transRows
    in
        {name = stateName
        ,transitions =transes }

--List.map convertToTransition transRows
createStateStructure: PlantUmlData -> State
createStateStructure umlData =
    let
        uniqList = uniqueStates umlData.transitionTable
    -- for each state in the uniqueList we create a state
       List.map (\state -> { state = state
                           ,createTransitions state umlData.transitionTable
                         }) uniqList
    in





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
