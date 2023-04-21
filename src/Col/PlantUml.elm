module Col.PlantUml exposing (stringToHex,test)
import Base16 exposing (decode, encode)
import Char
-- import String
plantUmlUrl = "http://www.plantuml.com/plantuml/uml/~h"
test="http://www.plantuml.com/plantuml/uml/~h407374617274756d6c0a416c6963652d3e426f62203a204920616d207573696e67206865780a40656e64756d6c"

stringToHex : String -> Result String String
stringToHex str =
        str
            |> String.toList
            |> List.map Char.toCode
            |> encode
    -- in
    -- str
    --     |> String.toList
    --     |> List.map Char.toCode
    --     |> List.map (\code -> String.fromInt code |> String.toHex)
    --     |> String.concat
