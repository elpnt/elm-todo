import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MAIN


main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }



-- MODEL


type alias Todo = String


type alias Model =
  { newTodo : Todo
  , todoList : List Todo
  }


init : Model
init =
  Model "" []



-- UPDATE


type Msg
  = NoOp
  | UpdateForm Todo
  | UpdateTodoList (List Todo)
  | ClearAll (List Todo)


update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp ->
      model

    UpdateForm todo ->
      { model | newTodo = todo }

    UpdateTodoList todolist ->
      { model | todoList = model.newTodo :: todolist }

    ClearAll todolist ->
      { model | todoList = [] }


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "ToDo App" ]
    , input
        [ type_ "text"
        , placeholder "Add TODO"
        , onInput UpdateForm
        ]
        []
    , button [ onClick ( UpdateTodoList model.todoList ) ] [ text "Add" ]
    , viewTodoList model.todoList
    , button [ onClick ( ClearAll model.todoList ) ] [ text "Clear All" ]
    ]


viewTodoList : List Todo -> Html Msg
viewTodoList todolist =
  ul [] ( List.map viewTodo todolist )


viewTodo : Todo -> Html Msg
viewTodo todo =
  li [] [ text todo ]
