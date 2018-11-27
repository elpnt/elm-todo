import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2)
import Html.Keyed as Keyed
import Json exposing (Decode)



-- MAIN


main =
  Browser.document
    { init = init
    , view = \model -> { title = "HELLO TODO", body = [ view model ] }
    , update = update
    , subscriptions = \_ -> Sub.none
    }



-- MODEL


type alias Entry =
  { content : String
  , id : Int
  , completed : Bool
  }


newEntry : String -> Int -> Entry
newEntry content id =
  { content = content
  , id = id
  , completed = False
  }


type alias Model =
  { field : String
  , entries : List Entry
  , uid : Int
  }


initialModel : Model
initialModel =
  { field = ""
  , entries = []
  , uid = 0
  }



init : () -> ( Model, Cmd Msg )
init _ =
  ( initialModel
  , Cmd.none
  )



-- UPDATE


type Msg
  = NoOp
  | UpdateField String
  | UpdateEntries
  | Check Int
  | ClearCompleted
  | ClearAll


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

    UpdateEntries ->
      ( { model | uid = model.uid + 1
                , field = ""
                , entries =
                    if String.isEmpty model.field then
                      model.entries
                    else
                      model.entries ++ [ newEntry model.field model.uid ]
        }
      , Cmd.none
      )

    Check id ->
      let
        updateEntry entry =
          if entry.id == id then
            { entry | completed = not entry.completed }
          else
            entry
      in
        ( { model | entries = List.map updateEntry model.entries }
        , Cmd.none
        )

    ClearCompleted ->
      ( { model | entries = List.filter (\entry -> not entry.completed )
      model.entries }
      , Cmd.none
      )


    ClearAll ->
      ( { model | entries = [] }
      , Cmd.none
      )



-- VIEW


view : Model -> Html Msg
view model =
  div
    [ class "todo-app" ]
    [ header
        []
        [ h1 [] [ text "ToDo App by Elm" ]
        , lazy viewInput model
        , p [ class "input-area" ]
            [ button
                [ onClick ( UpdateEntries) ]
                [ text "Add" ]
            ]
        ]
    , div
        [ class "active-entries" ]
        [ lazy2 viewEntries False model.entries ]
    , div
        [ class "completed-entries" ]
        [ lazy2 viewEntries True model.entries ]
    , div
        [ class "control" ]
        [ button [ onClick ( ClearCompleted ) ] [ text "Clear Completed Task" ]
        , button [ onClick ( ClearAll ) ] [ text "Clear All" ]
        ]
    ]

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decod.succeed msg
            else
                Decod.fail "not ENTER"
    in
        on "keydown" (Decod.andThen isEnter keyCode)


viewInput : Model -> Html Msg
viewInput model =
  Keyed.node "p"
    [ class "input-area" ]
    [ ( "aaa"
      , input
          [ type_ "text"
          , placeholder "Add new task here..."
          , autofocus True
          , value model.field
          , onInput UpdateField
					, onEnter UpdateEntries
          ]
          []
      )
    ]


viewEntries : Bool -> List Entry -> Html Msg
viewEntries isCompleted entries =
  Keyed.ul
    []
    ( entries
        |> List.filter (\entry -> entry.completed == isCompleted)
        |> List.map viewKeyedEntry
    )


viewKeyedEntry : Entry -> ( String, Html Msg )
viewKeyedEntry entry =
  ( String.fromInt entry.id, lazy viewEntry entry )


viewEntry : Entry -> Html Msg
viewEntry entry =
  let
    inputId = String.fromInt entry.id
  in
    li []
       [ div []
             [ input
                [ class "check"
                , id inputId
                , type_ "checkbox"
                , checked entry.completed
                , onClick ( Check entry.id )
                ]
                []
             , label
                [ for inputId ]
                [ text entry.content ]
             ]
       ]
