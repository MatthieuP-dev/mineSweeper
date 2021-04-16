module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src,style,class)
import Mine
import Html.Events as Events
import List.Extra

type alias Case =
    { isMine : Bool, revealed : Bool, x : Int, y : Int }

type alias Model =
    {
    board : List (List Case)
    , height : Int
    , width : Int
    , mines : List Mine.Mine
    }

generateBoard : List Mine.Mine -> List (List Case)
generateBoard mineList =
  let
    board = generateBoardHelper 19
  in
    List.foldl (helper) board mineList

generateBoardHelper : Int -> List (List Case)
generateBoardHelper int =
  List.map (\a ->List.map (\b -> { isMine = False, revealed = False, x = a, y = b }) (List.range 0 int)) (List.range 0 int)

helper : (Int, Int) -> List (List Case) -> List (List Case)
helper ( a, b ) caseList =
      case List.Extra.getAt a caseList of
        Nothing -> caseList
        Just (cases) -> List.Extra.setAt a (List.Extra.updateAt b (\x_ -> {isMine = True, revealed = False, x = a, y = b}) cases) caseList

revealCase : Case -> List (List Case) -> List (List Case)
revealCase case_ caseList =
    let
      (a, b) = (case_.x , case_.y)
    in
      case List.Extra.getAt a caseList of
        Nothing -> caseList
        Just (cases) -> List.Extra.setAt a (List.Extra.updateAt b (\x_ -> {isMine = case_.isMine, revealed = True, x = a, y = b}) cases) caseList

isRevealCase : Case -> Bool
isRevealCase case_ =
    case_.revealed

exampleGenerateRandomMines : Cmd Msg
exampleGenerateRandomMines =
    Mine.generateRandomMines
        { width = 19
        , height = 19
        , minMines = 10
        , maxMines = 30
        , initialX = 0
        , initialY = 0
        }
        MinesGenerated

viewCase : Case -> Html Msg
viewCase case_ =
  Html.button
    [ Events.onClick(Reveal case_), style "width" "17px", style "height" "17px", style "background-color" "light-grey"]
    [ if case_.revealed then
        if case_.isMine then
          text "💣"
        else
          text "1"
      else
        text ""
    ]


init : ( Model, Cmd Msg )
init =
    ( {board = [[]], height = 20, width = 20, mines = []}, exampleGenerateRandomMines )


type Msg
    = MinesGenerated (List ( Int, Int ))
    |Reveal Case



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      MinesGenerated value -> ({model | mines = value, board = generateBoard value}, Cmd.none)
      Reveal case_ -> let newBoard =  revealCase case_ model.board in ( {model | board = newBoard}, Cmd.none)

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
