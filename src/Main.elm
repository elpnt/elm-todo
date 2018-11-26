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


type alias Model =
  { newTodo : String
  -- , todoList : List String
  }


init : Model
init =
  -- Model "" []
  Model ""


-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newTodo ->
      { model | newTodo = newTodo }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Input your TODO", value model.newTodo, onInput Change
    ] []
    , div [] [ text model.newTodo ]]


