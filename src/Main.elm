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


type alias Task = String


type alias Model =
  { newTask : Task
  , taskList : List Task
  }


init : Model
init =
  Model "" []



-- UPDATE


type Msg
  = NoOp
  | Add Task
  | UpdateTaskList (List Task)


update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp ->
      model

    Add task ->
      { model | newTask = task }

    UpdateTaskList tasklist ->
      { model | taskList = model.newTask :: tasklist }


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "ToDo App" ]
    , input
        [ type_ "text"
        , placeholder "Add TODO"
        , onInput Add
        ]
        []
    , button [ onClick ( UpdateTaskList model.taskList ) ] [ text "Add" ]
    , viewTaskList model.taskList
    ]


viewTaskList : List Task -> Html Msg
viewTaskList tasklist =
  ul [] ( List.map viewTask tasklist )


viewTask : Task -> Html Msg
viewTask task =
  li [] [ text task ]
