port module Pottypott exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    String

diagram: String
diagram = """
@startuml

[*] --> State1
State1 --> [*]
State1 : this is a string
State1 : this is another string

State1 -> State2
State2 --> State3: EventHej
State3 --> [*]


@enduml
"""

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendDataToJS ]
            [ text "Send Data to JavaScript" ]
        , br [] []
        , br [] []
        , text ("Data received from JavaScript: " ++ model)
        ]


type Msg
    = SendDataToJS
    | ReceivedDataFromJS Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendDataToJS ->
            ( model, sendData diagram)

        ReceivedDataFromJS data ->
            ( data, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveData ReceivedDataFromJS


port sendData : String -> Cmd msg


port receiveData : (Model -> msg) -> Sub msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( "", Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
