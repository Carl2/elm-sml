port module Main exposing (main, update,  Msg(..))

--import Col.TableDef as Def exposing ()

import Browser
import Col.CppData as Cpp
import Html exposing (Html, button, code, div, input, pre, table, td, text, tr,span,img,option,select)
import Html.Keyed as Keyed
import Html.Attributes exposing (..)
import Html.Events exposing (onInput,onClick,onBlur)
import Col.PlantUml as PU
import Col.ModelData as MD exposing (Model,Machine,TableDataRow,RowData,convertToStringList,init,getActiveMachine,getMachineNames,getRootName,updateActiveMachineTableData)
import Col.Default as DF


-- Ports to javascript
port sendDiagram : String -> Cmd msg
port highlightCode : () -> Cmd msg
port openCompilerExplorer : String -> Cmd msg
port downloadFile : { filename : String, content : String } -> Cmd msg

type Msg
    = UpdateField Int Int String
      | UpdateSelection Int String
      | RenameMachine Int String
      | AddRow
      | DelRow
      | MakeUmlDiagram
      | UpdateMainContent String
      | OpenCompilerExplorer
      | DownloadCode
      | AddMachine
      | RemoveMachine Int
      | SwitchMachine Int







update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        UpdateField rowIndex fieldIndex newValue ->
            let
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
                (updateActiveMachineTableData model (updateRowAt rowIndex fieldIndex newValue), highlightCode ())

        RenameMachine idx newName ->
            renameMachine idx newName model

        AddRow ->
            let
                addNewRow tableData =
                    let
                        newIndex = List.length tableData
                        newRow = { rowIndex = newIndex, selected = "No Special", data = MD.defaultRowData }
                    in
                    tableData ++ [newRow]
                newModel = updateActiveMachineTableData model addNewRow
            in
            (newModel, Cmd.batch [highlightCode (), sendDiagram <| createPlantUmlDiagram newModel])

        DelRow ->
            let
                removeLastRow tableData =
                    List.take ((List.length tableData) - 1) tableData
                newModel = updateActiveMachineTableData model removeLastRow
            in
            (newModel, Cmd.batch [highlightCode (), sendDiagram <| createPlantUmlDiagram newModel])

        MakeUmlDiagram ->
            (model, sendDiagram <| createPlantUmlDiagram model)

        UpdateMainContent str ->
            ({model | mainContent = str}, Cmd.none )

        UpdateSelection rowIdx select ->
            let newModel = MD.updateSelected model rowIdx select
            in (newModel, Cmd.batch [highlightCode (), sendDiagram <| createPlantUmlDiagram newModel])

        OpenCompilerExplorer ->
            (model, openCompilerExplorer (generateFullCode model))

        DownloadCode ->
            (model, downloadFile 
                { filename = getRootName model ++ ".hpp"
                , content = generateFullCode model
                })

        AddMachine ->
            let
                newMachine = { name = "NewMachine", tableData = List.map (\i -> { rowIndex = i, selected = "No Special", data = MD.defaultRowData }) (List.range 0 4) }
                newModel = { model | machines = model.machines ++ [newMachine], activeMachine = List.length model.machines }
            in
            (newModel, Cmd.batch [highlightCode (), sendDiagram <| createPlantUmlDiagram newModel])

        RemoveMachine idx ->
            let
                -- Don't allow removing the root machine (index 0)
                newMachines = if idx > 0 then
                                  List.take idx model.machines ++ List.drop (idx + 1) model.machines
                              else
                                  model.machines
                newActive = Basics.min model.activeMachine (List.length newMachines - 1)
                            |> Basics.max 0
                newModel = { model | machines = newMachines, activeMachine = newActive }
            in
            (newModel, Cmd.batch [highlightCode (), sendDiagram <| createPlantUmlDiagram newModel])

        SwitchMachine idx ->
            let newModel = { model | activeMachine = idx }
            in (newModel, sendDiagram <| createPlantUmlDiagram newModel)




renameMachine: Int -> String -> Model -> (Model, Cmd msg)
renameMachine idx newName model =
    let
        isRootMachine = idx == 0
        prevRootName = getRootName model

        updateMachineAt machines =
            List.indexedMap
                (\i machine ->
                    if i == idx then
                        { machine | name = newName }
                    else
                        machine
                )
                machines

        newModel = { model | machines = updateMachineAt model.machines }

        -- If renaming the root machine, also update mainContent
        finalModel =
            if isRootMachine then
                let
                    prevContent = DF.smlStr ++ prevRootName
                in
                { newModel | mainContent = String.replace prevContent (DF.smlStr ++ newName) newModel.mainContent }
            else
                newModel
    in
    (finalModel, highlightCode ())


view : Model -> Html Msg
view model =
    let
        activeMachine = getActiveMachine model
    in
    div []
        [ makeTabBar model
        , case activeMachine of
            Just machine ->
                div []
                    [ makeMachineNameInput model
                    , table [] (makeModelTable model machine)
                    , button [onClick AddRow] [ text "+"]
                    , button [onClick DelRow] [ text "-"]
                    ]
            Nothing ->
                div [] [text "No machine selected"]
        , makeCodeOutput model
        , makeEventOutput model
        , makeMainOutput model
        , button [onClick MakeUmlDiagram] [text "Make Uml Diagram" ]
        , button [onClick OpenCompilerExplorer, style "margin-left" "10px"] [text "Compiler Explorer" ]
        , button [onClick DownloadCode, style "margin-left" "10px"] [text "Download Code" ]
        ]


-------------------------------------------------------------------------------
--                              Tab Bar                                      --
-------------------------------------------------------------------------------

makeTabBar : Model -> Html Msg
makeTabBar model =
    let
        makeTab idx machine =
            let
                isActive = idx == model.activeMachine
                tabStyle = if isActive then
                               [style "background-color" "#4a90d9", style "color" "white", style "font-weight" "bold"]
                           else
                               [style "background-color" "#e0e0e0", style "color" "black"]
                baseStyle = [style "padding" "8px 16px"
                            , style "border" "1px solid #ccc"
                            , style "border-bottom" "none"
                            , style "cursor" "pointer"
                            , style "margin-right" "2px"
                            , style "display" "inline-block"
                            ]
                removeBtn =
                    if idx > 0 then
                        [span [onClick (RemoveMachine idx), style "margin-left" "8px", style "cursor" "pointer"] [text "x"]]
                    else
                        []
            in
            span (baseStyle ++ tabStyle ++ [onClick (SwitchMachine idx)])
                ([text machine.name] ++ removeBtn)

        tabs = List.indexedMap makeTab model.machines
        addBtn = button [onClick AddMachine, style "margin-left" "4px"] [text "+"]
    in
    div [style "margin-bottom" "10px", style "border-bottom" "2px solid #4a90d9"]
        (tabs ++ [addBtn])


makeMachineNameInput: Model -> Html Msg
makeMachineNameInput model =
    div [ ] [
         text "Machine Name: "
        ,input [ type_ "text"
               , placeholder "Machine Name"
               , value (getActiveMachine model |> Maybe.map .name |> Maybe.withDefault "")
               , Html.Events.onInput (RenameMachine model.activeMachine)
               , onBlur MakeUmlDiagram] []
        ]

-------------------------------------------------------------------------------
--                     MakeUml string diagram from model                     --
-------------------------------------------------------------------------------

createPlantUmlDiagram: Model -> String
createPlantUmlDiagram model =
    if List.length model.machines > 1 then
        PU.makeNestedSystemString model.machines
    else
        case List.head model.machines of
            Just machine ->
                let
                    uniqueStates = MD.getAllStates machine
                    sys = PU.genSystem machine.name uniqueStates (PU.transformTR2Transition machine.tableData)
                in
                PU.makeSystemString sys
            Nothing ->
                ""

-------------------------------------------------------------------------------
--                    Generate full code for Compiler Explorer               --
-------------------------------------------------------------------------------
generateFullCode: Model -> String
generateFullCode model =
    let
        -- Collect all string lists from all machines for event/guard/action headers
        allStringLists = List.concatMap MD.convertToStringList model.machines
        eventHeader = Cpp.makeEventHeader allStringLists
        structs = Cpp.generateAllStructs model.machines
        mainContent = model.mainContent
    in
        Cpp.includeHeader ++ eventHeader ++ "\n" ++ structs ++ "\n" ++ mainContent

-------------------------------------------------------------------------------
--                              Make code output                             --
-------------------------------------------------------------------------------
makeCodeOutput: Model -> Html msg
makeCodeOutput model =
    let
        cppStr = Cpp.generateAllStructs model.machines
    in
        div [class "code-toolbar"]
            [Keyed.node "pre" [class "line-numbers"]
                 [(Cpp.includeHeader ++ cppStr, code [class "language-cpp"
                       , id "cpp-output"
                       , style "width" "940px"
                       , style "height" "auto"
                       , style "max-height" "400px"
                       , style "overflow" "auto"
                       ] [text (Cpp.includeHeader ++ cppStr)])
                 ]
            ]

makeEventOutput: Model -> Html msg
makeEventOutput model =
    let
        allStringLists = List.concatMap MD.convertToStringList model.machines
        eventCode = "// This could be placed in a header file\n" 
                    ++ (Cpp.makeEventHeader allStringLists)
    in
    div [class "code-toolbar"]
        [Keyed.node "pre" [class "line-numbers"]
             [(eventCode, code [class "language-cpp"
                   , id "event-output"
                   , style "width" "940px"
                   , style "height" "auto"
                   ] [text eventCode])
             ]
        ]

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
--                                    Main                                    --
-------------------------------------------------------------------------------


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


        lowerSpecial = String.toLower special
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
                               , value (Maybe.withDefault "" field)
                               , disabled <| isDisabled fieldIndex tableDataRow.selected
                               , placeholder (getPlaceHolderText fieldIndex)
                               , Html.Events.onInput
                                      (\newValue -> UpdateField
                                           rowIndex
                                           fieldIndex
                                           newValue)
                               , onBlur MakeUmlDiagram

                               ]
                               []
                         ]
                    ) fields ++ (onSpecial rowIndex)



isInternalTransition : TableDataRow -> Bool
isInternalTransition tableDataRow =
    let
        hasStart = tableDataRow.data.startState /= Nothing && tableDataRow.data.startState /= Just ""
        hasNoEnd = tableDataRow.data.endState == Nothing || tableDataRow.data.endState == Just ""
        notSpecial = String.toLower tableDataRow.selected == "no special"
        hasContent = tableDataRow.data.event /= Nothing && tableDataRow.data.event /= Just ""
                     || tableDataRow.data.guard /= Nothing && tableDataRow.data.guard /= Just ""
                     || tableDataRow.data.action /= Nothing && tableDataRow.data.action /= Just ""
    in
    hasStart && hasNoEnd && notSpecial && hasContent


makeModelTable: Model -> Machine -> List (Html Msg)
makeModelTable model machine =
    let
        rowStyle tableDataRow =
            if isInternalTransition tableDataRow then
                [style "background-color" "#B45309", title "Internal transition"]
            else
                []

        internalLabel tableDataRow =
            if isInternalTransition tableDataRow then
                [td [style "color" "white", style "font-size" "11px", style "padding-left" "8px"]
                    [text "internal"]]
            else
                []

        forEachRow tableDatas = List.indexedMap (\rowIndex tableData -> tr (rowStyle tableData) (forEachField rowIndex tableData ++ internalLabel tableData) ) tableDatas
    in
        makeHeader ++ [Html.tbody [] (forEachRow machine.tableData)]





makeHeader: List (Html msg)
makeHeader =
    [
     Html.caption [] [Html.text "Generation of Statemachine"]
    ,Html.thead [] [
         Html.tr [] [
             Html.th [style "background-color" "black", style "color" "white"] [
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
            ,Html.th [style "background-color" "#374151", style "color" "white"] [
                  Html.text "Special"
                 ]
            ]
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
