module React exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model = { showMsecs : Int, clicked : Bool,  clickedMsecs: Int }

initialModel : Int -> Model
initialModel showMsecs =
    { showMsecs = showMsecs
    , clicked = False
    , clickedMsecs = 0
    }


init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel(0)
  , Cmd.none
  )


type Msg
  = Start
  | NewMsecs Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Start ->
      ( model
      , Random.generate NewMsecs (Random.int 10 200)
      )

    NewMsecs msecs ->
      ( initialModel(msecs) 
      , Cmd.none
      )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (String.fromInt  model.showMsecs) ]
    -- , h1 [] [ text (model.clicked)]
    , h1 [] [ text (String.fromInt  model.clickedMsecs)]
    , button [ onClick Start ] [ text "Start" ]
    ]
