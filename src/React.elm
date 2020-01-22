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


type alias Point = { x: Int, y: Int, visible: Bool }
type alias Model = { showTime : Float, clicked : Bool,  clickedTime: Float, point: Maybe Point }

newModel : Float -> Maybe Point -> Model
newModel showTime point =
    { showTime = showTime
    , clicked = False
    , clickedTime = 0
    , point = point
    }

initialModel : Model
initialModel  = newModel 0 Nothing

newPoint : Int -> Int -> Point
newPoint x y =
    { x = x, y = y, visible = True}

drawPoint : Point -> Html Msg
drawPoint point =
    div [] [ text (String.fromInt point.x)
           , text (String.fromInt point.y)
           ]

randomPoint =
    Random.pair (Random.int 0 10) (Random.int 0 20)

randomTime =
    Random.float 500 2000

randomModel =
    Random.pair randomTime randomPoint


drawModel : Model -> Html Msg
drawModel model =
    case model.point of
        Just point ->
            div [] [
                 text "Point:"
                , drawPoint point
                , text (if model.clicked then "clicked" else "not clicked")
                ]

        Nothing -> div [] []

init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel
  , Cmd.none
  )


type Msg
  = Start
  | Stop
  | NewModel (Float, (Int, Int))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Start ->
      ( model
      , Random.generate NewModel randomModel
      )

    Stop ->
        ({ model | clicked = True }
        , Cmd.none
        )

    NewModel (time, ( x, y)) ->
        ( newModel time  (Just (newPoint x y))
        , Cmd.none
        )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (String.fromFloat  model.showTime) ]
    -- , h1 [] [ text (model.clicked)]
    , h1 [] [ text (String.fromFloat  model.clickedTime) ]
    , drawModel(model)
    , button [ onClick Start ] [ text "Start" ]
    , button [ onClick Stop ] [ text "Click" ]
    ]

