module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Http
import Result


type alias Model =
    { words : List String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { words = [] }, getWords )


type Msg
    = GotWords (Result String String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
    div [] [ text "Hi" ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


getWords : Cmd Msg
getWords =
    Http.get
        { url = wordsUrl
        , expect = Http.expectString (\res -> GotWords (Result.mapError (\_ -> "Error") res))
        }


wordsUrl : String
wordsUrl =
    "https://gist.githubusercontent.com/shmookey/b28e342e1b1756c4700f42f17102c2ff/raw/ed4c33a168027aa1e448c579c8383fe20a3a6225/WORDS"
