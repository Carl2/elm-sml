module Main exposing (main)



-- Main
main =
    Browser.sandbox{ init = init, update = update, view = view}

type alias Model =
    { StartState : String
    ,EndState: String
               ,
    }


init: Model
init =
    {
        val: "Not implemented yet"
    }


type Msg =
    Change String


update: Msg -> Msg -> Model
update msg model =
    case msg of
        Change newContent -> { model | val = newContent}


view: Model -> Html Msg
