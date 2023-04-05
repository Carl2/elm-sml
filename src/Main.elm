module Main exposing (main)
import Col.Table as Tbl exposing (..)
import Html exposing (Html, text, table, tr, td)
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
        startState= "Not implemented yet"
    ,endState= "N/A"
    }


type Msg =
    Change String


update: Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent -> { model | startState = newContent}


view: Model -> Html Msg
view model =
    Tbl.make_tbl
