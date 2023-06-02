module Col.CppData exposing (make_cpp_data,make_fsm_row
                            ,makeFsmRowTable,defaultName,makeConstexprClass,makeEventHeader
                            ,makeFsmRowFromModel)
import String.Interpolate exposing(interpolate)
import Array exposing (fromList,get)
import List.Extra as ListExtra
import Debug
import Col.ModelData as MD exposing (Model,TableDataRow,RowData)


isEntryStr: List (String,String)
isEntryStr =
    [ ("onentry","sml::on_entry<_>")
    ,("on_entry","sml::on_entry<_>")
    ,("sml::on_entry<_>","sml::on_entry<_>") ]

isExitStr: List (String, String)
isExitStr = [("onexit","sml::on_entry<_>")
            ,("on_exit","sml::on_entry<_>")
            ,("sml::on_entry<_>","sml::on_entry<_>")]

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

make_fsm_row : Int -> String -> String -> String -> String -> String -> Result String String
make_fsm_row lineNr start end event guard action =
    let
        updateAtIndex idx val lst =
            List.indexedMap
                (\i x ->
                     if i == idx then
                         val
                     else
                         x
                )
                lst
        interpolateString = "{0}   {1}   {2}   {3}   {4}"
        startWithPre = if lineNr == 0 then
                           "*" ++ start
                       else
                           "," ++ start
        args = [startWithPre, event, guard, action, end]
    in
        if String.isEmpty start then
            Err "Error: Start state is mandatory"
        else
            let
                args0 = if isNotEmpty end then
                            updateAtIndex 4 ("= " ++ (handleEvents end)) args
                        else
                            args
                args1 = if isNotEmpty event then
                           updateAtIndex 1 ("+ event<" ++ event ++">") args0
                       else
                           args0
                args2 = if isNotEmpty guard then
                            updateAtIndex 2 ("[" ++ guard ++"]") args1
                        else
                            args1
                args3 = if isNotEmpty action then
                            updateAtIndex 3 ("/ (" ++ action ++")") args2
                        else
                            args2
            in
                Ok (interpolate interpolateString args3)



makeFsmRow: Int -> List String -> Maybe String
makeFsmRow lineNr lstStr =
    case lstStr of
        [ start, end, ev, guard, action ] ->
            case make_fsm_row lineNr start end ev guard action of
                Ok row -> Just row
                Err err -> Debug.log err Nothing
        _ -> Nothing




makeFsmRowTable: List (List String ) -> String
makeFsmRowTable lstLstStr =
    let
        --Will create a List (Maybe str)
        lstMaybeStr = List.indexedMap makeFsmRow lstLstStr
        -- Now we concatenate the strings (not)
        concatenate_str maybeStr prev = case maybeStr of
                                            Just row -> Debug.log ("new row"++row) (row ++ "\n        " ++ prev)
                                            Nothing -> prev
    in
        List.foldr concatenate_str "" lstMaybeStr



handleOnSpecial: String -> List (String,String)-> Maybe String
handleOnSpecial event specialKeys =
    specialKeys
        |> List.filter (\(key,val) -> String.toLower event == key  )
        |> List.head
        |> Maybe.map Tuple.second

handleEvents: String -> String
handleEvents event =
    case handleOnSpecial event isExitStr of
                   Just exitKey -> exitKey
                   Nothing -> event
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



makeFsmRowFromData: RowData -> Int -> Bool -> String
makeFsmRowFromData rowData rowIdx special =
    let
        specialStr maybeStr maybeOther= if special == True then
                                            Maybe.withDefault "" maybeOther
                                        else
                                            (Maybe.withDefault "" maybeStr)


        resStr =  make_fsm_row rowIdx (Maybe.withDefault "" rowData.startState )
                     (specialStr rowData.endState Nothing)
                     (specialStr rowData.event (Just "sml::on_entry(_)"))
                     (specialStr rowData.guard Nothing)
                     (Maybe.withDefault "" rowData.action)
    in
        case resStr of
            Ok str ->  str ++ "\n        "
            Err str -> ""


makeFsmFromRowTable: TableDataRow -> String
makeFsmFromRowTable tblDataRow =
    let
        specialStr = Debug.log "selected " (String.toLower tblDataRow.selected)
    in
        if specialStr == "no special" then
            makeFsmRowFromData tblDataRow.data  tblDataRow.rowIndex False
        else
            makeFsmRowFromData tblDataRow.data tblDataRow.rowIndex True






makeFsmRowFromModel: MD.Model -> String
makeFsmRowFromModel model =
    List.map makeFsmFromRowTable model.tableData
        |> String.concat
