module Main exposing (main)
import Col.Table as Tbl exposing (..)
import Col.TableDef as Def exposing(..)
import Html exposing (Html, text, table, tr, td, input,div,button,code,pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Browser

type alias Model =
    { tableData : List (List String)
    }

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
                updateAtIndex idx val lst =
                    List.indexedMap (\i x -> if i == idx then val else x) lst

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
        , pre [] [
               code [class "language-cpp"] [text <|
                                                """
                                                 /* To be replaced by generated data */
                                                 int main()
                                                 {

                                                 std::cout << \"Hello\" << std::endl;
                                                 }
                                                 """]
              ]
        ]
-------------------------------------------------------------------------------
--                                    Old                                    --
-------------------------------------------------------------------------------
-- Main
main =
    Browser.sandbox{ init = init, update = update, view = view}
