module Main exposing (main)

import Array
import Browser
import Browser.Events as BEvents
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Decode as JD
import Result


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- TYPES


type alias Model =
    { words : List String, typedChars : Array.Array Word }


type Msg
    = GotWords (Result String String)
    | KeyPress Key
    | Toggle Int


type Key
    = Valid Char
    | Backspace
    | IgnoreKey


type alias Word =
    { index : Int, char : String, status : Status }


type Status
    = NotInWord
    | NotInPosition
    | InPosition



-- APP


init : () -> ( Model, Cmd Msg )
init _ =
    ( { words = [], typedChars = Array.empty }, getWords )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Toggle index ->
            ( { model
                | typedChars =
                    Array.indexedMap
                        (\idx wrd ->
                            if idx == index - 1 then
                                { wrd | status = toggleStatus wrd.status }

                            else
                                wrd
                        )
                        model.typedChars
              }
            , Cmd.none
            )

        KeyPress key ->
            case key of
                Valid char ->
                    ( { model | typedChars = Array.push (Word (Array.length model.typedChars + 1) (String.toLower <| String.fromChar char) NotInWord) model.typedChars }, Cmd.none )

                Backspace ->
                    ( { model
                        | typedChars = Array.slice 0 -1 model.typedChars
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GotWords res ->
            case res of
                Err e ->
                    let
                        _ =
                            Debug.log "error" e
                    in
                    ( model, Cmd.none )

                Ok wrds ->
                    ( { model
                        | words = String.lines wrds
                      }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    div
        [ Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(5,48px)"
        , Attr.style "gap" "10px"
        ]
    <|
        List.map viewWord (Array.toList model.typedChars)


subscriptions : Model -> Sub Msg
subscriptions _ =
    BEvents.onKeyUp (JD.map KeyPress keyDecoder)



-- UTILS and stuff
-- function to get words that we'll use as our database


keyDecoder : JD.Decoder Key
keyDecoder =
    let
        toKey val =
            if val == "Backspace" then
                Backspace

            else if val == " " then
                IgnoreKey

            else
                case String.uncons val of
                    Just ( char, "" ) ->
                        Valid char

                    _ ->
                        IgnoreKey
    in
    JD.map toKey (JD.field "key" JD.string)


getWords : Cmd Msg
getWords =
    Http.get
        { url = wordsUrl
        , expect = Http.expectString (\res -> GotWords (Result.mapError (\_ -> "Error") res))
        }


wordsUrl =
    "https://gist.githubusercontent.com/shmookey/b28e342e1b1756c4700f42f17102c2ff/raw/ed4c33a168027aa1e448c579c8383fe20a3a6225/WORDS"



-- function to view a word


viewWord : Word -> Html Msg
viewWord word =
    let
        bgColor =
            case word.status of
                NotInWord ->
                    "gainsboro"

                NotInPosition ->
                    "moccasin"

                InPosition ->
                    "yellowgreen"
    in
    div
        [ Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "width" "44px"
        , Attr.style "height" "44px"
        , Attr.style "border" ("1px solid " ++ bgColor)
        , Attr.style "background" bgColor
        , Attr.style "text-transform" "uppercase"
        , Attr.style "cursor" "default"
        , Events.onClick (Toggle word.index)
        ]
        [ text word.char ]


toggleStatus : Status -> Status
toggleStatus status =
    case status of
        NotInWord ->
            NotInPosition

        NotInPosition ->
            InPosition

        InPosition ->
            NotInWord
