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


-- | policyParams: e.g. ", sml::logger<my_logger>, sml::defer_queue<std::deque>"
-- | policyConstructorArg: e.g. "logger" or ""
makeMain: String -> List String -> String -> String -> String
makeMain name contextTypes policyParams policyConstructorArg =
    let
        -- Logger instance declaration (if logger policy present)
        loggerDecl = if not (String.isEmpty policyConstructorArg) then
                         "    my_logger " ++ policyConstructorArg ++ ";\n"
                     else ""

        -- Context variable declarations
        ctxDecls =
            List.indexedMap
                (\i t -> "    " ++ t ++ " " ++ makeContextVarName contextTypes i ++ "{};\n")
                contextTypes
                |> String.concat

        -- Constructor args: logger first, then context vars
        loggerArgList = if not (String.isEmpty policyConstructorArg) then [policyConstructorArg] else []
        ctxArgList =
            List.indexedMap
                (\i _ -> makeContextVarName contextTypes i)
                contextTypes
        allArgs = loggerArgList ++ ctxArgList
        argsStr = String.join ", " allArgs

        smDecl = "    " ++ smlStr ++ name ++ policyParams ++ "> sm{"
                 ++ argsStr ++ "};"

        output = loggerDecl ++ ctxDecls ++ smDecl
    in
    interpolate mainStr [String.trim output]
