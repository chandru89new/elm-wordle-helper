module Main exposing (..)

import Array
import Array.Extra as Array
import Browser
import Browser.Events as BEvents
import Html exposing (Html, div, text)
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
                        | words = String.lines wrds |> List.concatMap (String.split " ")
                      }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    div
        [ Attr.style "padding" "2rem"
        , Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "repeat(2, 500px)"
        , Attr.style "grid-gap" "2rem"
        ]
        [ div [] [ viewPlayground model ]
        , div [] [ viewResults model ]
        ]


viewResults : Model -> Html Msg
viewResults model =
    div [ Attr.style "border" "1px dotted gray", Attr.style "padding" "1rem" ]
        [ div [ Attr.style "margin-bottom" "1rem" ] [ text "Candidates:" ]
        , div
            [ Attr.style "whitespace" "wrap" ]
            [ text (String.join ", " (List.filter (isWordACandidate model.typedChars) model.words)) ]
        ]


viewPlayground : Model -> Html Msg
viewPlayground model =
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
    "https://raw.githubusercontent.com/madalynrose/Words/master/assets/5-letter.txt"



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


hasExcludedChar : Array.Array Word -> String -> Bool
hasExcludedChar excludedChars word =
    Array.any (\c -> c.status == NotInWord && String.contains c.char word) excludedChars


hasCharNotInPosition : { char : String, index : Int } -> String -> Bool
hasCharNotInPosition { char, index } word =
    if String.contains char word then
        word
            |> String.split ""
            |> Array.fromList
            |> Array.indexedMap
                (\idx c -> c == char && (idx + 1) == index)
            |> Array.any ((==) True)
            |> not

    else
        False


hasCharNotInPositionForAllChars : Array.Array { char : String, index : Int } -> String -> Bool
hasCharNotInPositionForAllChars xs word =
    Array.all (\x -> hasCharNotInPosition x word) xs


hasCharInPosition : { char : String, index : Int } -> String -> Bool
hasCharInPosition { char, index } word =
    if String.contains char word then
        word
            |> String.split ""
            |> Array.fromList
            |> Array.indexedMap
                (\idx c -> c == char && (idx + 1) == index)
            |> Array.any ((==) True)

    else
        False


hasCharInPositionForAllChars : Array.Array { char : String, index : Int } -> String -> Bool
hasCharInPositionForAllChars xs word =
    Array.all (\x -> hasCharInPosition x word) xs


convertWordIntoIndexedChar : Word -> { char : String, index : Int }
convertWordIntoIndexedChar { char, index } =
    if modBy 5 index == 0 then
        { char = char, index = 5 }

    else
        { char = char, index = modBy 5 index }


isWordACandidate : Array.Array Word -> String -> Bool
isWordACandidate wrds wordToCompare =
    if hasExcludedChar wrds wordToCompare then
        False

    else
        let
            wordsInPosition =
                Array.filter (\w -> w.status == InPosition) wrds

            wordsNotInPosition =
                Array.filter (\w -> w.status == NotInPosition) wrds
        in
        hasCharNotInPositionForAllChars (Array.map convertWordIntoIndexedChar wordsNotInPosition) wordToCompare && hasCharInPositionForAllChars (Array.map convertWordIntoIndexedChar wordsInPosition) wordToCompare



-- tests and misc.


testChars =
    Array.fromList
        [ { char = "f", index = 1 }
        , { char = "a", index = 2 }
        , { char = "t", index = 3 }
        ]


testWords =
    Array.fromList
        [ Word 1 "s" NotInPosition
        , Word 2 "p" NotInWord
        , Word 3 "p" NotInWord
        , Word 4 "l" NotInWord
        , Word 5 "e" NotInWord
        , Word 6 "f" NotInWord
        , Word 7 "a" NotInWord
        , Word 8 "c" NotInWord
        , Word 9 "t" NotInPosition
        , Word 10 "s" NotInPosition
        ]
