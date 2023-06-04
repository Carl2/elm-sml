port module Main exposing (main, update,  Msg(..))

--import Col.TableDef as Def exposing ()

import Browser
import Col.CppData as Cpp
import Html exposing (Html, button, code, div, input, pre, table, td, text, tr,span,img,option,select)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick)
import Col.PlantUml as PU
import Col.ModelData as MD exposing(Model,TableDataRow,RowData,convertToStringList,init)
import Col.Default as DF


-- Port to javascript
port sendDiagram : String -> Cmd msg


-- type alias TableDataRow = { rowIndex : Int
--                           ,selected: String
--                           ,data : List String
--                           }


-- type alias Model =
--     { tableData : List TableDataRow
--     ,systemName : String
--     ,mainContent : String
--     }

type Msg
    = UpdateField Int Int String
      | UpdateSelection Int String
      | UpdateMachineName String
      | AddRow
      | DelRow
      | MakeUmlDiagram
      | UpdateMainContent String







update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        UpdateField rowIndex fieldIndex newValue ->
            let
                -- This is a function that takes an index and a new value and updates the 'data' field of the TableData object at that index.

                updateRowAt rowIdx fIndex value tableRows =
                    List.indexedMap
                        (\i tableDataRow ->
                             if i == rowIdx then
                                 {tableDataRow | data = MD.updateDataAtIndex fIndex value tableDataRow.data}
                             else
                                 tableDataRow -- No change
                        )
                        tableRows
            in
                ({ model | tableData = updateRowAt rowIndex fieldIndex newValue model.tableData }, Cmd.none)

        UpdateMachineName str -> updateMachineName str model
        AddRow ->
            let
                newIndex = List.length model.tableData
                newRow =  { rowIndex = newIndex, selected = "No Special", data = MD.defaultRowData }
            in
                ({model | tableData = model.tableData ++ [newRow]}, Cmd.none)
        DelRow ->
            ({model | tableData = List.take ((List.length model.tableData) - 1) model.tableData }, Cmd.none)
        MakeUmlDiagram ->
            (model, sendDiagram <| createPlantUmlDiagram model)
        UpdateMainContent str ->
            ({model | mainContent = str}, Cmd.none )
        UpdateSelection rowIdx select ->
            (MD.updateSelected model rowIdx select,Cmd.none)






updateMachineName: String -> Model -> (Model, Cmd msg)
updateMachineName name model =
    let
        prevContent = DF.smlStr ++ model.systemName
        newModel = {model | systemName = name
                   , mainContent = (String.replace prevContent (DF.smlStr ++ name) model.mainContent)
                   }

    in
    (newModel, Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ makeSystemNameInput model
        ,table [] (makeModelTable model)
        ,button [onClick AddRow] [ text "+"]
        ,button [onClick DelRow] [ text "-"]
        ,makeCodeOutput model
        ,makeEventOutput model
        ,makeMainOutput model
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
    let
        data = convertToStringList mdl
    in
        data
        |> PU.convertTable mdl.systemName
        |> PU.createSystem
        |> PU.makeSystemString

-------------------------------------------------------------------------------
--                              Make code output                             --
-------------------------------------------------------------------------------
makeCodeOutput: Model -> Html msg
makeCodeOutput model =
    let
        smlClass = Cpp.makeConstexprClass <|  convertToStringList model
        cppStr = Cpp.makeFsmRowFromModel model
               |> Cpp.make_cpp_data  smlClass model.systemName
               |> text

    in
        pre [id "language-cpp"] [code [class "language-cpp"
                                      , style "width" "940px"  -- set width
                                      , style "height" "200px"  -- set height
                                      ] [cppStr]] --[cppStr]

makeEventOutput: Model -> Html msg
makeEventOutput model =
    div [] [pre [] [code [style "width" "940px"  -- set width
                         , style "height" "200px"  -- set height
                         ]
                        [text "// This could be placed in a header file"
                        , text (Cpp.makeEventHeader <| convertToStringList model)
                        ] ]]

makeMainOutput: Model -> Html Msg
makeMainOutput model =
    div [] [Html.textarea [value model.mainContent
                          , style "width" "940px"  -- set width
                          , style "height" "200px"  -- set height
                          , placeholder "c++20 code"
                          , Html.Events.onInput UpdateMainContent
                          ] []
           ]
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
isDisabled: Int -> String -> Bool
isDisabled fieldIdx special =
    let
        specialDisable idx = case idx of
                                 0 -> False
                                 1 -> True
                                 2 -> True
                                 3 -> True --Can i have a guard on "On_entry?"
                                 4 -> False
                                 _ -> True


        lowerSpecial = Debug.log "Special :" (String.toLower special)
    in
        if lowerSpecial /= "no special" then
            specialDisable fieldIdx
        else
            False


forEachField: Int -> TableDataRow -> List (Html Msg)
forEachField rowIndex tableDataRow =
    let
        onSpecial rowIdx =
            [ select [  onInput (\selected -> UpdateSelection rowIdx selected) ]
                  [option [ value "No special" ] [ text "No Special" ]
                  , option [ value "on entry" ] [ text "On Entry" ]
                  , option [ value "on exit" ] [ text "On Exit" ]
                  ]
            ]



        rowData = tableDataRow.data
        fields = [rowData.startState, rowData.endState, rowData.event, rowData.guard, rowData.action]
    in
    List.indexedMap (\fieldIndex field -> td []
                         [ input
                               [ type_ "text"
                               , disabled <| isDisabled fieldIndex tableDataRow.selected
                               , placeholder (getPlaceHolderText fieldIndex)
                               , Html.Events.onInput
                                     (\newValue -> UpdateField
                                          rowIndex
                                          fieldIndex
                                          newValue)

                               ]
                               []
                         ]
                    ) fields ++ (onSpecial rowIndex)



makeModelTable: Model -> List (Html Msg)
makeModelTable model =
    let


        forEachRow tableDatas = List.indexedMap (\rowIndex tableData -> tr [] (forEachField rowIndex tableData) ) tableDatas
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
    ,Html.th [style "background-color" "white", style "color" "black"] [
          Html.text "Special"
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
