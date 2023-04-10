module Col.Table exposing ( makeInput,makeTableRow, ViewInputItem)

-- import Html exposing (Html, text, table, tr, td)
import Html exposing (Html, Attribute, div, input, text, table , tr , td)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)







type alias ViewInputItem msg =
    { input_type : String
    , placeholder : String
    , value : String
    , toMsg : (String -> msg )
    , html : List (Html msg)
    }

type alias TableRowItem msg =
    { startState : ViewInputItem msg
    ,endState: ViewInputItem msg
    ,guard: ViewInputItem msg
    ,action: ViewInputItem msg
    }


makeInput:ViewInputItem msg -> Html msg
makeInput view =
    input [type_ view.input_type, placeholder view.placeholder, value view.value, onInput view.toMsg] view.html


tableData: ViewInputItem msg -> Html msg
tableData inputData =
    td [] [(makeInput inputData )]


makeTableRow: String -> List (ViewInputItem msg)  -> Html msg
makeTableRow rowNr tableRowItem =
        tr []  ((text rowNr) :: (List.map (\item -> tableData item) tableRowItem ))


-- makeTable: List (ViewInputItem msg) -> Html msg
-- makeTable tblRows =
--     let
--         htmlRows = List.map (\tblRow -> makeTableRow tblRow) tblRows
--     in
--     table [] htmlRows
