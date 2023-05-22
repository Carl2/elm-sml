port module Main exposing (main, update, Model, Msg(..))

--import Col.TableDef as Def exposing ()

import Browser
import Col.CppData as Cpp exposing (make_cpp_data, make_fsm_row,makeFsmRowTable)
import Col.Table as Tbl exposing (..)
import Html exposing (Html, button, code, div, input, pre, table, td, text, tr,span,img)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Col.PlantUml as PU


type alias Model =
    { tableData : List (List String),
          systemName : String
    }

-- Port to javascript
port sendDiagram : String -> Cmd msg

-- 5 rows with 5 fields. List (List String)
init : () -> (Model, Cmd Msg)
init _ =
    ({ tableData = List.repeat 5 (List.repeat 5 "")
    ,systemName = Cpp.defaultName
    },Cmd.none)


type Msg
    = UpdateField Int Int String
      | UpdateMachineName String
      | AddRow
      | DelRow
      | MakeUmlDiagram


update : Msg -> Model -> (Model, Cmd msg)
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
                ({ model | tableData = updateRowAt rowIndex fieldIndex newValue model.tableData },Cmd.none)
        UpdateMachineName str -> ({ model | systemName = str}, Cmd.none)
        AddRow ->
            ({model | tableData = List.append  model.tableData  [(List.repeat 5 "")]},Cmd.none)
        DelRow ->
            ({model | tableData = List.take ((List.length model.tableData) - 1) model.tableData }, Cmd.none)
        MakeUmlDiagram ->
            (model, sendDiagram <| createPlantUmlDiagram model)



view : Model -> Html Msg
view model =
    div []
        [ makeSystemNameInput model
        ,table [] (makeModelTable model)
        ,button [onClick AddRow] [ text "+"]
        ,button [onClick DelRow] [ text "-"]
        ,makeCodeOutput model
        ,makeEventOutput model
        ,button [onClick MakeUmlDiagram] [text "Make Uml Diagram" ]

        ]


makeSystemNameInput: Model -> Html Msg
makeSystemNameInput model =
    div [ ] [
         text "Statemachine Name: "
        ,input [ type_ "text"
               , placeholder "StateMachine Name"
               , Html.Events.onInput UpdateMachineName] []
        ]

-------------------------------------------------------------------------------
--                     MakeUml string diagram from model                     --
-------------------------------------------------------------------------------
createPlantUmlDiagram: Model -> String
createPlantUmlDiagram mdl =
    mdl.tableData
        |> PU.convertTable mdl.systemName
        |> PU.createSystem
        |> PU.makeSystemString

-------------------------------------------------------------------------------
--                              Make code output                             --
-------------------------------------------------------------------------------
makeCodeOutput: Model -> Html msg
makeCodeOutput model =
    let
        smlClass = Cpp.makeConstexprClass model.tableData
        cppStr = makeFsmRowTable model.tableData
               |> make_cpp_data  smlClass model.systemName
               |> text

    in
        pre [id "language-cpp"] [code [class "language-cpp"] [cppStr]] --[cppStr]

makeEventOutput: Model -> Html msg
makeEventOutput model =
    div [] [pre [] [code []
                        [text "// This could be placed in a header file"
                         ,text (Cpp.makeEventHeader model.tableData) ] ]]

-------------------------------------------------------------------------------
--                                    Old                                    --
-------------------------------------------------------------------------------
-- Main
-- subscriptions : Model -> Sub Msg
-- subscriptions _ =
--     receiveData ReceivedDataFromJS

main =
    Browser.element {
            init = init
                ,update = update
                ,view = view
                ,subscriptions = \_ -> Sub.none
        }


-------------------------------------------------------------------------------
--                              Make html table                              --
-------------------------------------------------------------------------------
makeModelTable: Model -> List (Html Msg)
makeModelTable model =
    let
        forEachField rowIndex row = List.indexedMap (\fieldIndex _ -> td []
                                                         [ input
                                                               [ type_ "text"
                                                               , placeholder (getPlaceHolderText fieldIndex)
                                                               , Html.Events.onInput
                                                                     (\newValue -> UpdateField
                                                                          rowIndex
                                                                          fieldIndex
                                                                          newValue)
                                                               ]
                                                               []
                                                         ]
                                                    ) row
        forEachRow rows = List.indexedMap (\rowIndex row -> tr [] (forEachField rowIndex row)) rows
    in
        (List.append makeHeader (forEachRow model.tableData))






makeHeader: List (Html msg)
makeHeader =
    [
     Html.caption [] [Html.text "Generation of Statemachine"]
    ,Html.th [style "background-color" "black", style "color" "white"] [
           Html.text "Start State"
          ]
    ,Html.th [style "background-color" "black", style "color" "white"] [
           Html.text "End State"
          ]
    ,Html.th [style "background-color" "blue", style "color" "white"] [
           Html.text "Event"
          ]
    ,Html.th [style "background-color" "red", style "color" "white"] [
           Html.text "Guard"
          ]
    ,Html.th [style "background-color" "green", style "color" "white"] [
           Html.text "Action"
          ]
    ]


getPlaceHolderText: Int -> String
getPlaceHolderText idx =
    if idx == 0 then
        "Start state name"
    else if idx == 1 then
        "End state name"
    else if idx == 2 then
        "Event Name"
    else if idx == 3 then
        "Guard (fn) name"
    else if idx == 4 then
        "Action (fn) name"
    else
        "Unknown"
