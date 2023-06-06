module Col.CppData exposing (make_cpp_data,
                            defaultName,makeConstexprClass,makeEventHeader
                            ,makeFsmRowFromModel,makeFsmRowFromData)
import String.Interpolate exposing(interpolate)
import Array exposing (fromList,get)
import List.Extra as ListExtra
import Debug
import Col.ModelData as MD exposing (Model,TableDataRow,RowData,Selected(..))




endStateStr = "X"
defaultName = "StateMachine"
constexprFmt = "constexpr static auto {0} = sml::state<class {0}>;"
eventFmt ="""
struct {0} {};
"""

cpp_data: String
cpp_data = """
#include <boost/sml.hpp>

// Create a header file with {0}.hpp for example
// This was created with help of elm-sml (by Carl Olsen)

namespace sml = boost::sml;

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
-- So this returns a  String
-------------------------------------------------------------------------------

interpolateStates: String -> String
interpolateStates state =

    "    " ++ (interpolate constexprFmt [state]) ++ "\n"


makeConstexprClass: List (List String) -> String
makeConstexprClass lstLstStr =
    let
        uniqStateLst = uniqueFields lstLstStr firstTwo

        checkStr row prev = case row of
                                "" -> prev
                                "X" -> prev
                                _ -> if String.startsWith "*" row then
                                         prev ++ (interpolateStates (String.dropLeft 1 row))
                                     else
                                         prev ++ (interpolateStates row)
    in
        List.foldl (\rowStr prev -> checkStr rowStr prev) "" uniqStateLst

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


makeEventHeader: List (List String) -> String
makeEventHeader lstLstStr =
    lstLstStr
        |> eventLst
        |> List.foldl (\ev str ->  (interpolateEvent ev) ++ str ) ""



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






makeFsmRowFromData: RowData -> Int -> MD.Selected -> String
makeFsmRowFromData rowData rowIdx selected =
    let

        resStr = makeFsmRowInternal rowIdx [
                  handleStateTransition selected <| StartState <| rowData.startState
                 ,handleStateTransition selected <| EndState <| rowData.endState
                 ,handleStateTransition selected <| Event <| rowData.event
                 ,handleStateTransition selected <| Guard <| rowData.guard
                 ,handleStateTransition selected <| Action <| rowData.action
                 ] selected
    in
        resStr


makeFsmFromRowTable: TableDataRow -> String
makeFsmFromRowTable tblDataRow =
        makeFsmRowFromData tblDataRow.data  tblDataRow.rowIndex
            <| Maybe.withDefault NO (MD.convertSelected tblDataRow.selected)



makeFsmRowFromModel: MD.Model -> String
makeFsmRowFromModel model =
    List.map makeFsmFromRowTable model.tableData
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
