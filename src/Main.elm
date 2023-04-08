module Main exposing (main)
import Col.Table as Tbl exposing (..)
import Col.TableDef as Def exposing(..)
import Html exposing (Html, text, table, tr, td, input,div,button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Browser

-- Main
main =
    Browser.sandbox{ init = init, update = update, view = view}


type alias RowItem =
    { rowNr : Int
    , colNr: Int
    , value: String
    }



type alias Model =
    {
        startState : RowItem
    ,endState: RowItem
    }


init: Model
init =
    {startState = { rowNr = 0,colNr = 0, value = ""}
    ,endState = { rowNr = 0, colNr = 0, value = ""}
    }



type Msg = StartState String
         | EndState String


updateModelValue: String -> RowItem -> RowItem
updateModelValue newVal row =
    {row | value = newVal}

update: Msg -> Model -> Model
update msg model =
    case msg of
        StartState newContent ->
            let
                row = updateModelValue newContent model.startState
            in
                { model | startState = row}

        EndState newContent ->
            let
                row = updateModelValue newContent model.endState
            in
                { model | endState = row}









view: Model -> Html Msg
view model =
    let

         tblList = [ Def.startStateInput StartState |> updateState model.startState.value
                   ,Def.endStateInput EndState |> updateState model.endState.value]

         tableRow = Tbl.makeTableRow tblList


    in
        div []
            [--Tbl.makeInput tbl
             table [] [tableRow]
            -- [input [ placeholder "Text to reverse", value model.startState, onInput Change ] []
            -- ,div [] [ text (String.reverse model.startState) ]
            -- ,Tbl.makeTable [tbl]
            -- ,

            --     Tbl.viewInput "text" "TBD" model.startState Change

            ]
