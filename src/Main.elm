module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Mine
import Html.Events as Events

type alias Case =
    { isMine : Bool, revealed : Bool }

type alias Model =
    {
    board : List (List Case)
    , height : Int
    , width : Int
    }


exampleGenerateRandomMines : Cmd Msg
exampleGenerateRandomMines =
    Mine.generateRandomMines
        { width = 100
        , height = 100
        , minMines = 10
        , maxMines = 30
        , initialX = 0
        , initialY = 0
        }
        MinesGenerated

viewCase : Case -> Html Msg
viewCase case_ =
  Html.button
    []
    []


init : ( Model, Cmd Msg )
init =
    ( {board = [[]], height = 100, width = 100}, exampleGenerateRandomMines )


type Msg
    = MinesGenerated (List ( Int, Int ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        ([ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , text "Implémentez le démineur !"
        , div []
          (List.foldl (\x a -> (List.map viewCase x) ++ a) [] model.board)
        ])


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = always init
        , update = update
        , subscriptions = always Sub.none
        }
