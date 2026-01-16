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



makeMain: String ->String
makeMain name=
    let
        output = smlStr ++ name ++ "> sm{};"
    in
    interpolate mainStr [output]
