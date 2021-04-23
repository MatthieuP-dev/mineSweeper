module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src,style,class)
import Mine
import Html.Events as Events
import List.Extra

type alias Case =
    { isMine : Bool, revealed : Bool, flag : Bool, x : Int, y : Int }

type alias Model =
    {
    board : List (List Case)
    , height : Int
    , width : Int
    , mines : List Mine.Mine
    , state : String
    , score : Int
    }

generateBoard : List Mine.Mine -> List (List Case)
generateBoard mineList =
  let
    board = generateBoardHelper 19
  in
    List.foldl (helper) board mineList

generateBoardHelper : Int -> List (List Case)
generateBoardHelper int =
  List.map (\a ->List.map (\b -> { isMine = False, revealed = False, flag = False , x = a, y = b }) (List.range 0 int)) (List.range 0 int)

helper : (Int, Int) -> List (List Case) -> List (List Case)
helper ( a, b ) caseList =
      case List.Extra.getAt a caseList of
        Nothing -> caseList
        Just (cases) -> List.Extra.setAt a (List.Extra.updateAt b (\x_ -> {isMine = True, revealed = False, flag = False, x = a, y = b}) cases) caseList

revealCaseOnClick : Case -> List (List Case) -> String -> List (List Case)
revealCaseOnClick case_ board state =
  if state == "Game in progress" && case_.flag==False then
    if (bombsAround case_ board) > 0 then reveal1Case case_ board
    else
      let
        newBoard = reveal1Case case_ board
        casesAround = getCaseAround case_ [] board
      in
        revealHelperRecursive casesAround newBoard
  else
    board

reveal1Case : Case -> List (List Case) -> List (List Case)
reveal1Case case_ board =
  if case_.flag==False then
    let
      (a, b) = (case_.x , case_.y)
    in
      case List.Extra.getAt a board of
        Nothing -> board
        Just (cases) -> List.Extra.setAt a (List.Extra.updateAt b (\x_ -> {isMine = case_.isMine, revealed = True, flag = False, x = a, y = b}) cases) board
  else
    board

getCase : (Int, Int) ->  List (List Case) -> Maybe Case
getCase ( a, b ) board =
    case List.Extra.getAt a board of
      Nothing -> Nothing
      Just (cases) -> List.Extra.getAt b cases

getCaseAround : Case -> List Case -> List (List Case) -> List Case
getCaseAround case_ cases board=
    let
      visits =[
        (case_.x-1, case_.y-1)
        , (case_.x, case_.y-1)
        , (case_.x-1, case_.y)
        , (case_.x+1, case_.y+1)
        , (case_.x+1, case_.y)
        , (case_.x, case_.y+1)
        , (case_.x+1, case_.y-1)
        , (case_.x-1, case_.y+1)]
    in
      List.foldl (\(a,b) acc ->
        let
          maybeCase = getCase ( a, b ) board
        in
          case maybeCase of
            Nothing -> acc
            Just (c) -> if (List.member c cases)== False && c.revealed == False then c::acc else acc) [] visits


revealHelperRecursive : List Case -> List (List Case) -> List (List Case)
revealHelperRecursive listCase board =
  case listCase of
    [] -> board
    case_::cases ->
      let
        newBoard = reveal1Case case_ board
      in
        if (bombsAround case_ board) > 0 then revealHelperRecursive cases newBoard
        else
          let
            newList = cases++(getCaseAround case_ cases newBoard)
          in
            revealHelperRecursive newList newBoard

flagCase : Case -> List (List Case) ->  String -> List (List Case)
flagCase case_ board state =
  if state == "Game in progress" then
    let
      (a, b) = (case_.x , case_.y)
    in
     case List.Extra.getAt a board of
      Nothing -> board
      Just (cases) -> List.Extra.setAt a (List.Extra.updateAt b (\x_ -> {isMine = case_.isMine, revealed = False, flag = True, x = a, y = b}) cases) board
    else
      board

bombsAround : Case -> List (List Case) -> Int
bombsAround case_ board =
    let
      visits =[
        (case_.x-1, case_.y-1)
        , (case_.x, case_.y-1)
        , (case_.x-1, case_.y)
        , (case_.x+1, case_.y+1)
        , (case_.x+1, case_.y)
        , (case_.x, case_.y+1)
        , (case_.x+1, case_.y-1)
        , (case_.x-1, case_.y+1)]
    in
      let
        visited_case = List.foldl (\(a,b) acc -> (visitHelper ( a, b ) board)::acc) [] visits
      in
        List.foldl (\c a-> if c.isMine then a+1 else a+0) 0 visited_case

visitHelper : (Int, Int) ->  List (List Case) -> Case
visitHelper ( a, b ) board =
  case List.Extra.getAt a board of
    Nothing -> { isMine = False, revealed = False, flag = False, x = a, y = b }
    Just (cases) -> case  List.Extra.getAt b cases of
      Nothing -> { isMine = False, revealed = False, flag = False, x = a, y = b }
      Just (case_) -> case_


exampleGenerateRandomMines : Cmd Msg
exampleGenerateRandomMines =
    Mine.generateRandomMines
        { width = 19
        , height = 19
        , minMines = 70
        , maxMines = 80
        , initialX = 0
        , initialY = 0
        }
        MinesGenerated

viewCase : List (List Case) -> Case -> Html Msg
viewCase board case_=
  Html.button
    [{-Events.onDoubleClick(Flag case_) ,-}Events.onClick(Reveal case_), style "width" "17px", style "height" "17px", style "background-color" "light-grey"]
    [ if case_.flag then text "🚩"
      else
        if case_.revealed then
          if case_.isMine then text "💣"
          else
            text (String.fromInt (bombsAround case_ board))
        else
          text ""
    ]

countScore : List (List Case) -> Model -> Int
countScore board model =
  if model.state == "Game in progress" then
    List.foldl (\x a -> List.foldl (\c y -> if c.revealed then y+1 else y ) a x) 0 board
  else
    model.score

init : ( Model, Cmd Msg )
init =
    ( {board = [[]], height = 20, width = 20, mines = [], state = "Game in progress", score = 0}, exampleGenerateRandomMines )

type Msg
    = MinesGenerated (List ( Int, Int ))
    |Reveal Case
    |Flag Case
    |Loose
    |Win
    |Reset

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      MinesGenerated value -> ({model | mines = value, board = generateBoard value}, Cmd.none)
      Reveal case_ ->
        let
          newBoard =  revealCaseOnClick case_ model.board model.state
        in
          if case_.isMine then ( {model | board = newBoard, state = "You loose"}, Cmd.none)
          else
            ( {model | board = newBoard, score = countScore newBoard model}, Cmd.none)
      Flag case_ -> ({model | board = flagCase case_ model.board model.state}, Cmd.none)

      Win -> ({model | state = "You win"}, Cmd.none)
      Loose -> ({model | state = "You loose"}, Cmd.none)
      Reset -> ( {board = [[]], height = 20, width = 20, mines = [], state = "Game in progress", score = 0}, exampleGenerateRandomMines )


view : Model -> Html Msg
view model =
    div []
        ([ h1 [] [ text "Démineur" ]
        , text "Play !"
        ,div [class "myButton"] [Html.button [Events.onClick Reset] [text "Reset"]]
        ,div [class "myButton"] [text (String.fromInt model.score)]
        ,div [class "myButton"] [text (model.state)]
        , div [class "myGrid"]
          (let board = model.board in (List.foldl (\x a -> (List.map (viewCase board) x) ++ a) [] model.board))
        ])


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = always init
        , update = update
        , subscriptions = always Sub.none
        }
