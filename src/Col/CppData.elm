module Col.CppData exposing (make_cpp_data,make_fsm_row,makeFsmRowTable)
import String.Interpolate exposing(interpolate)
import Array exposing (fromList,get)
import Debug




cpp_data: String
cpp_data = """
struct {0}
{
  auto operator()() const {
    using namespace sml;
    // clang-format off
    return make_transition_table(
        //-[CurrentState]---|------[Event]-----|---[Guard]----|--[Action]---|--Next State-----
        {1}
    );
    // clang-format on
  }
};
"""

make_cpp_data: String -> String -> String
make_cpp_data modelName str = interpolate cpp_data [modelName ,str]

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
        lstMaybeStr = List.indexedMap makeFsmRow lstLstStr --Will create a List (Maybe str)

                                                    -- Now we concatenate the strings (not)
        concatenate_str maybeStr prev = case maybeStr of
                                            Just row -> Debug.log ("new row"++row) (row ++ "\n        " ++ prev)
                                            Nothing -> prev
    in
        List.foldr concatenate_str "" lstMaybeStr
