module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src,style,class)
import Mine
import Html.Events as Events

type alias Case =
    { isMine : Bool, revealed : Bool }

type alias Model =
    {
    board : List (List Case)
    , height : Int
    , width : Int
    , mines : List Mine.Mine
    }

generateBoard : List Mine.Mine -> List (List Case)
generateBoard mineList =
  List.repeat 20 (List.repeat 20 ({isMine = False, revealed = False}))


exampleGenerateRandomMines : Cmd Msg
exampleGenerateRandomMines =
    Mine.generateRandomMines
        { width = 20
        , height = 20
        , minMines = 10
        , maxMines = 30
        , initialX = 0
        , initialY = 0
        }
        MinesGenerated

viewCase : Case -> Html Msg
viewCase case_ =
  Html.button
    [style "width" "15px", style "height" "15px"]
    []


init : ( Model, Cmd Msg )
init =
    ( {board = [[]], height = 20, width = 20, mines = []}, exampleGenerateRandomMines )


type Msg
    = MinesGenerated (List ( Int, Int ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      MinesGenerated value -> ({model | mines = value, board = generateBoard value}, Cmd.none)


view : Model -> Html Msg
view model =
    div []
        ([ h1 [] [ text "Démineur" ]
        , text "Jouez !"
        , div [class "myGrid"]
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
