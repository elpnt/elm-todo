import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)
import Html.Keyed as Keyed



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
  { field : Todo
  , id : Int
  , todoList : List Todo
  , doneList : List Todo
  }


initialModel : Model
initialModel =
  { field = ""
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
  | UpdateField Todo
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

    UpdateField todo ->
      ( { model | field = todo }
      , Cmd.none
      )

    UpdateTodoList todolist ->
      ( { model | todoList = model.field :: todolist
                , id = model.id + 1
                , field = "" }
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
    [ h1 [] [ text "ToDo App by Elm" ]
    , renderInput model
    , button [ onClick ( UpdateTodoList model.todoList ) ] [ text "Add" ]
    , viewTodoList model.todoList
    -- , viewDoneList model.doneList
    , button [ onClick ( ClearAll model.todoList ) ] [ text "Clear All" ]
    ]


renderInput : Model -> Html Msg
renderInput model =
  Keyed.node "div"
    []
    [ ( "a" -- String.fromInt model.id
      , input
          [ type_ "text"
          , placeholder "Something to do"
          , autofocus True
          , value model.field
          , onInput UpdateField
          ]
          []
      )
    ]


viewTodoList : List Todo -> Html Msg
viewTodoList todolist =
  ul [] ( List.map viewTodo todolist )


viewTodo : Todo -> Html Msg
viewTodo todo =
  li []
    [ div []
        [ text todo
        -- , button [ onClick ( CompleteTask 1 ) ] [ text "Done!" ]
        ]
    ]


viewDoneList : List Todo -> Html Msg
viewDoneList donelist =
  ul [] ( List.map viewDone donelist )


viewDone : Todo -> Html Msg
viewDone done =
  li [] [ text done ]
