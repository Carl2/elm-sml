module Col.CppData exposing (make_cpp_data,make_fsm_row,makeFsmRowTable)
import String.Interpolate exposing(interpolate)
import Array exposing (fromList,get)
import Debug




cpp_data: String
cpp_data = """
struct StateMachine
{
  auto operator()() const {
    using namespace sml;
    // clang-format off
    return make_transition_table(
        //-[CurrentState]---|------[Event]-----|---[Guard]----|--[Action]---|--Next State-----
        {0}
    );
    // clang-format on
  }
};
"""

make_cpp_data: String -> String
make_cpp_data str = interpolate cpp_data [str]

isNotEmpty : String -> Bool
isNotEmpty str =
    not (String.isEmpty str)

make_fsm_row : String -> String -> String -> String -> String -> Result String String
make_fsm_row start end event guard action =
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
        args = [start, event, guard, action, end]
    in
        if String.isEmpty start then
            Err "Error: Start state is mandatory"
        else
            let
                args0 = if isNotEmpty end then
                            updateAtIndex 4 ("= " ++ end) args
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



makeFsmRow: List String -> Maybe String
makeFsmRow lstStr =
    case lstStr of
        [ start, end, ev, guard, action ] ->
            case make_fsm_row start end ev guard action of
                Ok row -> Just row
                Err err -> Debug.log err Nothing
        _ -> Nothing




makeFsmRowTable: List (List String ) -> String
makeFsmRowTable lstLstStr =
    let
        lstMaybeStr = List.map makeFsmRow lstLstStr --Will create a List (Maybe str)
                                                    -- which will be
    -- Now we concatenate the strings (not)
        concatenate_str maybeStr prev = case maybeStr of
                                            Just row -> Debug.log ("new row"++row) (row ++ "\n        " ++ prev)
                                            Nothing -> prev
    in
        List.foldr concatenate_str "" lstMaybeStr
