import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2)
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
  | Complete Int
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

    Complete id ->
      let
        updateEntry entry =
          if entry.id == id then
            { entry | completed = True }
          else
            entry
      in
        ( { model | entries = List.map updateEntry model.entries }
        , Cmd.none
        )

    ClearAll ->
        ( { model | entries = [] }
        , Cmd.none
        )



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text "ToDo App by Elm" ]
    , lazy viewInput model
    , button [ onClick ( UpdateEntries) ] [ text "Add" ]
    , lazy2 viewEntries False model.entries  -- not done yet
    , lazy2 viewEntries True model.entries   -- completed!
    , button [ onClick ( ClearAll ) ] [ text "Clear All" ]
    ]


viewInput : Model -> Html Msg
viewInput model =
  Keyed.node "div"
    []
    [ ( "aaa"
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
  li []
     [ div []
           [ text entry.content
           , if entry.completed == False then
               button [ onClick ( Complete entry.id ) ] [ text "Done!" ]
             else
               div [] []
           ]
     ]
