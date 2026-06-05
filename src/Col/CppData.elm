module Col.CppData exposing (make_cpp_data, includeHeader,
                            defaultName,makeConstexprClass,makeEventHeader
                            ,makeFsmRowFromMachine,makeFsmRowFromData
                            ,isSubSM,generateAllStructs,makeContextParams
                            ,makeContextStructs)
import String.Interpolate exposing(interpolate)
import Array exposing (fromList,get)
import List.Extra as ListExtra
import Debug
import Col.ModelData as MD exposing (Machine,TableDataRow,RowData,Selected(..))



endStateStr = "X"
defaultName = "StateMachine"
constexprFmt = "constexpr static auto {0} = sml::state<class {0}>;"
wrapSubSm name = "sml::state<" ++ name ++ ">"

makeContextParams : List String -> String
makeContextParams contextTypes =
    case contextTypes of
        [] -> ""
        [single] -> ", " ++ single ++ "& ctx_"
        multiple ->
            List.indexedMap (\i t -> ", " ++ t ++ "& ctx_" ++ String.fromInt i) multiple
                |> String.concat


makeContextStructs : List String -> String
makeContextStructs contextTypes =
    let
        nonEmpty = List.filter (not << String.isEmpty) contextTypes
    in
    case nonEmpty of
        [] -> ""
        types ->
            List.map (\t -> "struct " ++ t ++ " {};\n") types
                |> String.concat
                |> (\s -> "\n" ++ s)
eventFmt ="""
struct {0} {};
"""

actionFmt ctxParams ="""
auto {0} = [](const auto& event""" ++ ctxParams ++ """) {};
"""

guardFmt ctxParams ="""
auto {0} = [](const auto& event""" ++ ctxParams ++ """) { return true; };
"""

includeHeader : String
includeHeader = """#include <boost/sml.hpp>

namespace sml = boost::sml;
"""

cpp_data: String
cpp_data = """
// Create a header file with {0}.hpp for example
// This was created with help of elm-sml (by Carl Olsen)

struct {0}
{
{1}
  auto operator()() const {
    using namespace sml;
    // clang-format off
    return make_transition_table(
        //-[CurrentState]---|------[Event]-----|---[Guard]----|--[Action]---|--Next State-----
        {2}
    );
    // clang-format on
  }
};
"""


make_cpp_data: String -> String -> String -> String
make_cpp_data stateClass modelName str =
    let
        fixedModelName = if String.isEmpty modelName then
                             defaultName
                         else
                             modelName
    in
    interpolate cpp_data [fixedModelName,stateClass ,str]

isNotEmpty : String -> Bool
isNotEmpty str =
    not (String.isEmpty str)



-------------------------------------------------------------------------------
--                             Make sml constexpr                            --
--  The idea is to make constexpr sml of the states.
--  For each row.
--    Get the start and end state (index 0,1) and if they are unique
--    Transform that into a string of type:
--                            constexpr static auto <state> = sml::state<class <state>>
--  For sub-SM references:
--                            constexpr static auto <SubSM> = sml::state<SubSM>
-- So this returns a  String
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--                         Sub-SM Detection                                  --
-------------------------------------------------------------------------------

isSubSM : List String -> String -> Bool
isSubSM machineNames stateName =
    List.member stateName machineNames


interpolateStates: List String -> String -> String
interpolateStates machineNames state =
    if isSubSM machineNames state then
        ""
    else
        "    " ++ (interpolate constexprFmt [state]) ++ "\n"


makeConstexprClass: List String -> List (List String) -> String
makeConstexprClass machineNames lstLstStr =
    let
        uniqStateLst = uniqueFields lstLstStr firstTwo

        checkStr row prev = case row of
                                "" -> prev
                                "X" -> prev
                                _ -> if String.startsWith "*" row then
                                         prev ++ (interpolateStates machineNames (String.dropLeft 1 row))
                                     else
                                         prev ++ (interpolateStates machineNames row)
    in
        List.foldl (\rowStr prev -> checkStr rowStr prev) "" uniqStateLst


-------------------------------------------------------------------------------
--                     Topological Sort & Multi-Struct Generation            --
-------------------------------------------------------------------------------

-- | Get the sub-SM dependencies of a machine (states that match other machine names)
getMachineDeps : List String -> Machine -> List String
getMachineDeps machineNames machine =
    let
        allStates = MD.getAllStates machine
    in
    List.filter (\s -> isSubSM machineNames s && s /= machine.name) allStates


-- | Topological sort of machines (leaf-first, root-last)
-- Returns (sorted machines, has cycle)
topologicalSort : List Machine -> (List Machine, Bool)
topologicalSort machines =
    let
        machineNames = List.map .name machines

        -- Simple iterative topological sort: repeatedly pick machines with no unresolved deps
        go remaining sorted =
            if List.isEmpty remaining then
                (sorted, False)
            else
                let
                    sortedNames = List.map .name sorted
                    -- A machine is ready if all its deps are already in sorted
                    isReady m =
                        getMachineDeps machineNames m
                            |> List.all (\dep -> List.member dep sortedNames)

                    (ready, notReady) = List.partition isReady remaining
                in
                if List.isEmpty ready then
                    -- Cycle detected: no machine can be resolved
                    (sorted ++ remaining, True)
                else
                    go notReady (sorted ++ ready)
    in
    go machines []


-- | Generate all structs in dependency order (leaf-first, root-last)
-- If a cycle is detected, emits an error comment
generateAllStructs : List Machine -> String
generateAllStructs machines =
    let
        machineNames = List.map .name machines
        (sortedMachines, hasCycle) = topologicalSort machines

        cycleComment = if hasCycle then
                           "// ERROR: circular sub-SM reference detected\n"
                       else
                           ""

        generateOne machine =
            let
                stringList = MD.convertToStringList machine
                stateClass = makeConstexprClass machineNames stringList
                fsmRows = makeFsmRowFromMachine machineNames machine
            in
            make_cpp_data stateClass machine.name fsmRows
    in
    cycleComment ++ (List.map generateOne sortedMachines |> String.join "\n")


firstTwo : List String -> List String
firstTwo list =
    case list of
        [stateStart,stateEnd,_,_,_] ->
            [ stateStart, stateEnd ]
        _ -> []

-------------------------------------------------------------------------------
--                        Need to create a unique list                       --
--   Each class can only be there once..
-------------------------------------------------------------------------------
uniqueFields : List (List String) -> (List String -> List String) -> List String
uniqueFields listOfLists fn =
    listOfLists
        |> List.concatMap fn
        |> ListExtra.unique



getEventFromLst lst = case lst of
                           [_,_,ev,_,_] -> ev -- Check if its empty
                           _ -> ""


eventLst: List (List String) -> List String
eventLst  listOflist = listOflist
                     |> List.foldl (\row prevEvent ->  (getEventFromLst row) :: prevEvent ) []
                     |> ListExtra.unique

interpolateEvent: String -> String
interpolateEvent event =
    if not (String.isEmpty event) then
        interpolate eventFmt [event]
    else
        ""

-------------------------------------------------------------------------------
--               Make Event header
--     All the unique events should become a struct {event}
-------------------------------------------------------------------------------


makeEventHeader: List String -> List (List String) -> String
makeEventHeader contextTypes lstLstStr =
    let
        ctxParams = makeContextParams contextTypes
        events = lstLstStr
            |> eventLst
            |> List.foldl (\ev str ->  (interpolateEvent ev) ++ str ) ""
        guards = lstLstStr
            |> guardLst
            |> List.foldl (\grd str -> (interpolateGuard ctxParams grd) ++ str) ""
        actions = lstLstStr
            |> actionLst
            |> List.foldl (\act str -> (interpolateAction ctxParams act) ++ str) ""
    in
        events ++ guards ++ actions


getActionFromLst : List String -> String
getActionFromLst lst = 
    case lst of
        [_,_,_,_,act] -> act
        _ -> ""


actionLst: List (List String) -> List String
actionLst listOflist = 
    listOflist
        |> List.foldl (\row prevAction -> (getActionFromLst row) :: prevAction) []
        |> ListExtra.unique


interpolateAction: String -> String -> String
interpolateAction ctxParams action =
    if not (String.isEmpty action) then
        interpolate (actionFmt ctxParams) [action]
    else
        ""


getGuardFromLst : List String -> String
getGuardFromLst lst = 
    case lst of
        [_,_,_,grd,_] -> grd
        _ -> ""


guardLst: List (List String) -> List String
guardLst listOflist = 
    listOflist
        |> List.foldl (\row prevGuard -> (getGuardFromLst row) :: prevGuard) []
        |> ListExtra.unique


interpolateGuard: String -> String -> String
interpolateGuard ctxParams guard =
    if not (String.isEmpty guard) then
        interpolate (guardFmt ctxParams) [guard]
    else
        ""



------------
-- ReMake --
------------
type StateTransitionType
    = StartState (Maybe String)
    | EndState (Maybe String)
    | Event (Maybe String)
    | Guard (Maybe String)
    | Action (Maybe String)

isMaybeEmptyStr: Maybe String -> Maybe String
isMaybeEmptyStr maybeStr =
    case maybeStr of
        Just str -> if String.isEmpty str then
                        Nothing
                    else
                        Just str
        Nothing -> Nothing

handleStateTransition: Selected -> StateTransitionType -> StateTransitionType
handleStateTransition selected stateType =
    let
        special sel mStr = case selected of
                               NO -> isMaybeEmptyStr mStr
                               ON_ENTRY -> Just "sml::on_entry<_>"
                               ON_EXIT ->  Just "sml::on_exit<_>"

        noStrForSpecial sel mStr = case sel of
                                       NO ->  isMaybeEmptyStr mStr
                                       _ -> Nothing
    in
        case stateType of
            StartState state -> StartState <| isMaybeEmptyStr state
            EndState state -> EndState <| noStrForSpecial selected state
            Event event -> Event <| special selected event
            Guard guard -> Guard <| noStrForSpecial selected guard
            Action act  -> Action <| isMaybeEmptyStr act





makeFsmRowFromData: List String -> RowData -> Int -> MD.Selected -> String
makeFsmRowFromData machineNames rowData rowIdx selected =
    let
        transformSubSM mState =
            case mState of
                Just s -> if isSubSM machineNames s then
                              Just (wrapSubSm s)
                          else
                              Just s
                Nothing -> Nothing

        resStr = makeFsmRowInternal rowIdx [
                  handleStateTransition selected <| StartState <| transformSubSM rowData.startState
                 ,handleStateTransition selected <| EndState <| transformSubSM rowData.endState
                 ,handleStateTransition selected <| Event <| rowData.event
                 ,handleStateTransition selected <| Guard <| rowData.guard
                 ,handleStateTransition selected <| Action <| rowData.action
                 ] selected
    in
        resStr


makeFsmFromRowTable: List String -> TableDataRow -> String
makeFsmFromRowTable machineNames tblDataRow =
        makeFsmRowFromData machineNames tblDataRow.data  tblDataRow.rowIndex
            <| Maybe.withDefault NO (MD.convertSelected tblDataRow.selected)



makeFsmRowFromMachine: List String -> Machine -> String
makeFsmRowFromMachine machineNames machine =
    List.map (makeFsmFromRowTable machineNames) machine.tableData
        |> String.concat

-------------------------------------------------------------------------------
-- Below this point is the construction of the fsmRow (remake)
-------------------------------------------------------------------------------
makeFsmRowInternal: Int -> List StateTransitionType -> Selected -> String
makeFsmRowInternal lineNr transition select =
    let
        resList = case transition of
                      [StartState startState, EndState end, Event ev, Guard guard, Action action] ->
                          [handleStartState lineNr startState
                          ,handleEvent ev select
                          ,handleGuard guard
                          ,handleAction action
                          ,handleEnd end]
                      _ -> [Err "Not all fields are ok"]
        strCat listOfResults = List.foldl (\prev this-> case (prev,this) of
                                                            (Ok prevStr,Ok val) -> Ok <| prevStr ++ val
                                                            (_,_) -> Err "Unbarable"
                                          ) (Ok "") (List.reverse listOfResults)
    in
        case strCat resList of
            Ok out -> out ++ "\n        "
            Err err -> Debug.log err ""


handleStartState: Int -> Maybe String -> Result String String
handleStartState lineNr mStr =
    let
        pre post = if lineNr == 0 then
                       "*" ++ post
                   else
                       "," ++ post
    in
        case mStr of
            Just str -> Ok (pre str)
            Nothing -> Err "No Startstate provided"



handleEvent: Maybe String -> Selected -> Result String String
handleEvent mStr select=
    let
        pre = case mStr of
                  Nothing -> ""
                  Just _ -> "+"
        evenExpandStr str = case str of
                                Nothing -> ""
                                Just ev -> "event<" ++ ev ++ ">"
    in
       case select of
           NO -> Ok <| handleSpace 25  (pre ++ (evenExpandStr mStr))
           _ -> Ok  <| handleSpace 25  (pre ++ Maybe.withDefault "" mStr)



handleGuard: Maybe String -> Result String String
handleGuard guard =
    case guard of
        Nothing -> Ok <| handleSpace 15 ""
        Just grd -> Ok <| handleSpace 15 ("[" ++ grd ++"]")


handleAction: Maybe String -> Result String String
handleAction action =
    case action of
        Nothing -> Ok <| handleSpace 15 ""
        Just act -> Ok <| handleSpace 15 "/ (" ++ act ++")"

handleEnd: Maybe String -> Result String String
handleEnd endState =
    case endState of
        Nothing -> Ok <| handleSpace 15 ""
        Just state -> Ok <| handleSpace 15 ("= "++state)
        --Just state -> Ok <| "     = " ++ state



handleSpace: Int -> String -> String
handleSpace space str =
    let
        len = String.length str
        spaces = String.repeat (space - len) " "
    in
        spaces ++ str
