module Main exposing (main)

--import Col.TableDef as Def exposing ()

import Browser
import Col.CppData as Cpp exposing (make_cpp_data, make_fsm_row)
import Col.Table as Tbl exposing (..)
import Html exposing (Html, button, code, div, input, pre, table, td, text, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type alias Model =
    { tableData : List (List String)
    }



-- 5 rows with 5 fields. List (List String)


init : Model
init =
    { tableData = List.repeat 5 (List.repeat 5 "") }


type Msg
    = UpdateField Int Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateField rowIndex fieldIndex newValue ->
            let
                -- This is a function that takes and index and a new val and replaces that in the list
                updateAtIndex idx val lst =
                    List.indexedMap
                        (\i x ->
                            if i == idx then
                                val

                            else
                                x
                        )
                        lst

                updateRowAt rowIdx fIndex value table =
                    List.indexedMap
                        (\i row ->
                            if i == rowIdx then
                                updateAtIndex fIndex value row

                            else
                                row
                        )
                        table
            in
            { model | tableData = updateRowAt rowIndex fieldIndex newValue model.tableData }


view : Model -> Html Msg
view model =
    div []
        [ table []
            (List.indexedMap
                (\rowIndex row ->
                    tr []
                        (List.indexedMap
                            (\fieldIndex _ ->
                                td []
                                    [ input
                                        [ type_ "text"
                                        , placeholder "Enter text"
                                        , Html.Events.onInput (\newValue -> UpdateField rowIndex fieldIndex newValue)
                                        ]
                                        []
                                    ]
                            )
                            row
                        )
                )
                model.tableData
            )
        , pre []
            [ code [ class "language-cpp" ]
                [ text <| make_cpp_data "apa" ]
            ]
        ]



-------------------------------------------------------------------------------
--                                    Old                                    --
-------------------------------------------------------------------------------
-- Main


main =
    Browser.sandbox { init = init, update = update, view = view }



-------------------------------------------------------------------------------
--                             Makes the cpp data output                     --
-------------------------------------------------------------------------------


cpp_data : Model -> Int -> String
cpp_data modl rowIdx =
    let
        maybeRow =
            List.drop rowIdx modl.tableData |> List.head
    in
    case maybeRow of
        Just row ->
            case row of
                [ start, end, ev, guard, action ] ->
                    case Cpp.make_fsm_row start end ev guard action of
                        Ok fsmRow ->
                            Cpp.make_cpp_data fsmRow
                        Err err ->
                            err
                _ ->
                    ""
        Nothing ->
            ""



-- cpp_data : Model -> Int -> String
-- cpp_data model rowIdx =
--     -- For now we just use the first row
--     case model.tableData of
--         (row :: _) ->
--             case row of
--                 [start,end,ev ,guard,action]  ->
--                     case Cpp.make_fsm_row start end ev guard action of
--                         Ok fsmRow -> Cpp.make_cpp_data fsmRow
--                         Err err   -> err
--                 _ -> ""
--         _ -> ""
