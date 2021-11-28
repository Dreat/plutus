module Main exposing (..)

import Browser
import Html exposing (Html, text, div, button, input, Attribute)
import Http
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, field, string, int, list)
import Json.Decode.Pipeline exposing (required, optional)

main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model = 
    { content : String
    , words: Words
    }

init : () -> (Model, Cmd Msg)
init _ = 
    (initModel
    , Http.get
        { url = "./hasla.json"
        , expect = Http.expectJson GotText parseWords
        }
    )

type alias Words =
    { words : List String }

parseWords : Decoder Words
parseWords =
    Json.Decode.succeed Words
    |> required "words" (list string)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

initModel : Model
initModel =
    Model "" (Words [])

type Msg = 
    Change String 
    | GotText (Result Http.Error Words)
    | Submit String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        Change newContent -> 
            ( { model | content = newContent }, Cmd.none )

        Submit text ->
            ( { model | content = (List.foldl (++) "" (model.words.words ++ [text])) }, Cmd.none )


        GotText result ->
            case result of
                Ok words ->
                    ( {model | words = words}, Cmd.none )

                Err _ ->
                    ( {model | content = "fail"}, Cmd.none )




view : Model -> Html Msg
view model =
    div []
    [ div [] [ text "Podaj haslo" ]
    , input [ placeholder "Nowe haslo", value model.content, onInput Change] []
    , div [] [ text model.content ]
    , button [ onClick (Submit model.content)] [ text "Dodaj hasuo" ]
    ]
