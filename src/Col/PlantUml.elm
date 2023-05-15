module Col.PlantUml exposing (convertTable,plantUmlDataToString)
import String.Interpolate exposing(interpolate)
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
        endState = if String.isEmpty row.endState then
                       row.startState
                   else
                       row.endState
    in
        if String.isEmpty row.startState then
           ""
        else
            row.startState ++ "->" ++ endState ++ ": " ++ row.event ++ guardStr ++ actionStr ++ "\n"

plantUmlDataToString : PlantUmlData -> String
plantUmlDataToString data =
    "@startuml\n\n" ++ getInitialStartState data
        ++ (data.transitionTable
           |> List.map transitionRowToString
           |> String.concat
           )
        ++ "@enduml\n"
