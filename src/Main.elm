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

type alias Model =
    {
        startState : String
    ,endState: String
    }


init: Model
init =
    {
        startState= ""
    ,endState= ""
    }

-- type alias RowItem =
--     { rowNr : Int
--     ,colNr : Int
--     ,val : String
--     }


type Msg = StartState String
         | EndState String


update: Msg -> Model -> Model
update msg model =
    case msg of
        StartState newContent -> { model | startState =  newContent}
        EndState newContent -> { model | endState =  newContent}








view: Model -> Html Msg
view model =
    let

         tblList = [ Def.startStateInput StartState |> updateState model.startState
                   ,Def.endStateInput EndState |> updateState model.endState]

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
