module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src,style,class)
import Mine
import Html.Events as Events
import List.Extra

-- Dimension de notre board et nombre de bombes
boardWidth : Int
boardWidth = 16

boardHeight : Int
boardHeight = 16

numberBombs : Int
numberBombs = 40

type alias Case =
    { isMine : Bool, revealed : Bool, flag : Bool, x : Int, y : Int }

type alias Model =
    {
    board : List (List Case)
    , height : Int
    , width : Int
    , mines : List Mine.Mine
    , state : String --Correspond à à l'état victoire, défaite ou en cours de jeu
    , score : Int --Correspond au nombre de case découvert
    }

--Permet de générer notre notre liste de liste de case qui sera notre board
generateBoard : List Mine.Mine -> List (List Case)
generateBoard mineList =
  let
    board = generateBoardHelper boardWidth boardHeight
  in
    List.foldl (helper) board mineList

generateBoardHelper : Int -> Int -> List (List Case)
generateBoardHelper w h =
  List.map (\a ->List.map (\b -> { isMine = False, revealed = False, flag = False , x = a, y = b }) (List.range 0 (w-1))) (List.range 0 (h-1))

--Ajoute les bombes dans le board
helper : (Int, Int) -> List (List Case) -> List (List Case)
helper ( a, b ) caseList =
      case List.Extra.getAt a caseList of
        Nothing -> caseList
        Just (cases) -> List.Extra.setAt a (List.Extra.updateAt b (\x_ -> {isMine = True, revealed = False, flag = False, x = a, y = b}) cases) caseList

--Révéle une case et d'autres cases de maniére recursive si la case à 0 bombe autour
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

--Révéle une case
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

--Renvoie la case à la position a,b
getCase : (Int, Int) ->  List (List Case) -> Maybe Case
getCase ( a, b ) board =
    case List.Extra.getAt a board of
      Nothing -> Nothing
      Just (cases) -> List.Extra.getAt b cases

--Renvoie toute les cases autours de case_, la liste de case pris en paramétre est là pour évité les doublon
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

--Renvoie le board avec toutes les cases qui doivent être révélés
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

--Met un flag sur une case mais elle n'est pas utilisé car la fonction onDoubleClick ne fonctionne pas trés bien
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

--Renvoie le nombre de bombe autour de case_
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
        { width = boardWidth-1
        , height = boardHeight-1
        , minMines = numberBombs
        , maxMines = numberBombs
        , initialX = 0
        , initialY = 0
        }
        MinesGenerated

viewCase : List (List Case) -> Case -> Html Msg
viewCase board case_=
  Html.button
    --la mise en place du flag est désactivé car je n'est pas trouvé le moyen de gérer via autre chose que le double clique et le double ne fonctionne pas bien
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

--Renvoie le score qui correspond au nombre de case révélés
countScore : List (List Case) -> Model -> Int
countScore board model =
  if model.state == "Game in progress" then
    List.foldl (\x a -> List.foldl (\c y -> if c.revealed then y+1 else y ) a x) 0 board
  else
    model.score

init : ( Model, Cmd Msg )
init =
    ( {board = [[]], height = boardHeight, width = boardWidth, mines = [], state = "Game in progress", score = 0}, exampleGenerateRandomMines )

type Msg
    = MinesGenerated (List ( Int, Int ))
    |Reveal Case
    |Flag Case
    |Reset

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      MinesGenerated value -> ({model | mines = value, board = generateBoard value}, Cmd.none)
      Reveal case_ ->
        let
          newBoard =  revealCaseOnClick case_ model.board model.state
        in
          if case_.isMine then ( {model | board = newBoard, state = "You loose"}, Cmd.none) --On perd quand on révéle une bombe
          else
            let
              newScore = countScore newBoard model
            in
              if newScore == boardHeight*boardWidth - numberBombs then ( {model | board = newBoard, state ="You win", score = countScore newBoard model}, Cmd.none) --on gagne si on réussi à révélé toutes les cases qui ne sont pas des bombes
              else
                ( {model | board = newBoard, score = countScore newBoard model}, Cmd.none)
      Flag case_ -> ({model | board = flagCase case_ model.board model.state}, Cmd.none)
      Reset -> ( {board = [[]], height = boardHeight, width = boardWidth, mines = [], state = "Game in progress", score = 0}, exampleGenerateRandomMines )


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
