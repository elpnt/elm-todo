import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MAIN


main =
  Browser.document
    { init = init
    , view = \model -> { title = "HELLO TODO", body = [ view model ] }
    , update = update
    , subscriptions = \_ -> Sub.none
    }



-- MODEL


type alias Todo = String


type alias Model =
  { newTodo : Todo
  , id : Int
  , todoList : List Todo
  , doneList : List Todo
  }


initialModel : Model
initialModel =
  { newTodo = ""
  , id = 0
  , todoList = []
  , doneList = []
  }


init : () -> ( Model, Cmd Msg )
init _ =
  ( initialModel
  , Cmd.none
  )



-- UPDATE


type Msg
  = NoOp
  | UpdateForm Todo
  | UpdateTodoList (List Todo)
  | CompleteTask Int
  | ClearAll (List Todo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model
      , Cmd.none
      )

    UpdateForm todo ->
      ( { model | newTodo = todo }
      , Cmd.none
      )

    UpdateTodoList todolist ->
      ( { model | todoList = model.newTodo :: todolist
                , id = model.id + 1 }
      , Cmd.none
      )

    CompleteTask uid ->
      ( { model | todoList = updateTodoListWhenCompleted uid model.todoList
                , doneList = "a" :: model.doneList }
      , Cmd.none
      )

    ClearAll todolist ->
      ( { model | todoList = [] }
      , Cmd.none
      )


updateTodoListWhenCompleted : Int -> List Todo -> List Todo
updateTodoListWhenCompleted idx todolist =
  (List.take idx todolist) ++ (List.drop idx todolist)


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
    , viewDoneList model.doneList
    , button [ onClick ( ClearAll model.todoList ) ] [ text "Clear All" ]
    ]


initialForm : Html Msg
initialForm =
  input
    [ type_ "text"
    , placeholder "Add TODO"
    , value ""
    , onInput UpdateForm
    ]
    []


viewTodoList : List Todo -> Html Msg
viewTodoList todolist =
  ul [] ( List.map viewTodo todolist )


viewTodo : Todo -> Html Msg
viewTodo todo =
  li []
    [ div []
        [ text todo
        , button [ onClick ( CompleteTask 1 ) ] [ text "Done!" ]
        ]
    ]


viewDoneList : List Todo -> Html Msg
viewDoneList donelist =
  ul [] ( List.map viewDone donelist )


viewDone : Todo -> Html Msg
viewDone done =
  li [] [ text done ]
