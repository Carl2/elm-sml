module Col.Default exposing (defaultName, makeMain,smlStr)
import String.Interpolate exposing(interpolate)

defaultName: String
defaultName = "StateMachine"



-------------------------------------------------------------------------------
--                             Make main function                            --
-- It would be nice to actually create the functions here too.
--
-------------------------------------------------------------------------------
smlStr = "sml::sm<"

mainStr="""
int main(int argc, char *argv[])
{
    {0}
    return 0;
}
"""


makeContextVarName : List String -> Int -> String
makeContextVarName contextTypes idx =
    if List.length contextTypes == 1 then
        "ctx_"
    else
        "ctx_" ++ String.fromInt idx


makeMain: String -> List String -> String
makeMain name contextTypes =
    let
        ctxDecls =
            List.indexedMap
                (\i t -> "    " ++ t ++ " " ++ makeContextVarName contextTypes i ++ "{};\n")
                contextTypes
                |> String.concat

        ctxArgs =
            List.indexedMap
                (\i _ -> makeContextVarName contextTypes i)
                contextTypes
                |> String.join ", "

        smDecl = "    " ++ smlStr ++ name ++ "> sm{"
                 ++ ctxArgs ++ "};"

        output = ctxDecls ++ smDecl
    in
    interpolate mainStr [String.trim output]
