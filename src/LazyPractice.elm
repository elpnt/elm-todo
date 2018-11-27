port module LazyPractice exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)
import Json.Decode as D
import Html.Keyed as Keyed
import  exposing (..)


main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }


-- MODEL

type alias Model
  = { field : String }


init : Model
init = { field = "" }


-- UPDATE

type Msg
  = NoOp
  | UpdateField String
  | Add


update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp ->
      model

    UpdateField str ->
      { model | field = str }

    Add ->
      { model | field = "" }



-- VIEW


view : Model -> Html Msg
view model =
  div []
      [ renderInput model
      , button [ onClick Add ] [ text "clear!" ]
      ]


viewInput : Model -> Html Msg
viewInput model =
  Keyed.node "div"
    []
    [ ( "a"
      , input
          [ type_ "text"
          , value ""
          ]
          []
      )
    ]
