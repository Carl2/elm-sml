module Col.TableDef exposing (endStateInput,startStateInput, updateState)
import Col.Table as Tbl exposing(..)

type alias MsgFn msg= (String -> msg)


-------------------------------------------------------------------------------
--                                State fields                               --
-------------------------------------------------------------------------------
stateInput: String -> MsgFn msg -> Tbl.ViewInputItem msg
stateInput str to_msg =
    {input_type="text"
    , placeholder= str
    , value=""
    , toMsg= to_msg
    ,html=[]}


startStateInput: MsgFn msg -> Tbl.ViewInputItem msg
startStateInput to_msg =
    stateInput "Start State" to_msg

endStateInput: MsgFn msg -> Tbl.ViewInputItem msg
endStateInput to_msg =
    stateInput "End state" to_msg


updateState:  String -> Tbl.ViewInputItem msg -> Tbl.ViewInputItem msg
updateState  str tblInput =
    { tblInput | value=str }
-- makeFields: List (ViewInputItem msg)
-- makeFields =
