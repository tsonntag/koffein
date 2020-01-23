module React exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Process
import Task
import Time exposing (..)
import Point exposing (..)
import Debug exposing (log)
import Utils exposing (boolToString)


main =
  Browser.element
    { init = init , update = update , subscriptions = subscriptions , view = view}


type alias Model = { maxRounds: Int
                   , rounds:    List Round
                   , current:   Round
                   , avg:       Maybe Int
                   }

type alias Round = { showAfter: Maybe Int
                   , startedAt: Maybe Int
                   , clickedAt: Maybe Int
                   , point:     Maybe Point
                   , show:      Bool
                   }

initialModel : Model
initialModel =
    { maxRounds = 0
    , rounds    = []
    , current   = initialRound
    , avg       = Nothing
    }

initialRound : Round
initialRound =
    { showAfter = Nothing
    , startedAt = Nothing
    , clickedAt = Nothing
    , point     = Nothing
    , show      = False
    }


drawInt : Maybe Int -> String
drawInt int = int |> Maybe.map String.fromInt |> Maybe.withDefault ""


drawRound : Round -> Html Msg
drawRound model
            = div []
              [ p  [] [ text "Point show after: "
                      , text <| drawInt model.showAfter
                      ]
              , p  [] [ text "Started: "
                      , text <| drawInt model.startedAt
                      ]
              , p  [] [ text "Clicked: "
                      , text <| drawInt model.clickedAt
                      ]
              , p  [] [ text "Duration: "
                      , text <| drawInt (duration model)
                      ]
              , div  [] [ case (model.show, model.point) of
                              (True, Just p) -> Html.map PointMsg (drawPoint p)
                              _ -> text ""
                        ]
              ]

duration  { clickedAt, startedAt, showAfter } =
    Maybe.map3 (\a b c -> a - b - c) clickedAt startedAt showAfter


after : Int -> Msg -> Cmd Msg
after time msg =
    Process.sleep (toFloat time) |> Task.perform (always msg)


startCmd : Cmd Msg
startCmd =
    Cmd.batch
        [ Task.perform    SetStartedAt Time.now
        , Random.generate SetPoint     (Random.pair (Random.int 0 10) (Random.int 0 20))
        , Random.generate SetShowAfter (Random.int 1000 5000)
        ]

type Msg
  = SetMaxRounds  String
  | Start
  | ShowPoint
  | SetClickedAt  Time.Posix
  | SetStartedAt  Time.Posix
  | SetShowAfter  Int
  | SetPoint      (Int, Int)
  | PointMsg      Point.Msg


updateCurrent : Model -> ( Round -> Round ) -> Model
updateCurrent model f =
   { model | current = f model.current }

init : () -> (Model, Cmd Msg)
init _ = ( initialModel, Cmd.none )



update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetMaxRounds ns ->
        ({ model | maxRounds = ns |> String.toInt |> Maybe.withDefault 0 }
        , Cmd.none )

    Start ->
        ( initialModel
        , startCmd )

    SetShowAfter showAfter ->
        ( updateCurrent model (\r -> { r | showAfter = Just showAfter})
        , after showAfter ShowPoint )

    SetClickedAt time ->
        ( updateCurrent model (\r -> { r | clickedAt = Just <| Time.posixToMillis time })
        , Cmd.none )

    SetStartedAt time ->
        ( updateCurrent model (\r -> { r | startedAt = Just <| Time.posixToMillis time })
        , Cmd.none)

    SetPoint (x, y) ->
        ( updateCurrent model (\r -> { r | point = Just (Point x y) })
        , Cmd.none )

    ShowPoint ->
        ( updateCurrent model (\r -> { r | show = True })
        , Cmd.none )

    PointMsg Point.Clicked ->
        ( model
        , Task.perform SetClickedAt Time.now)



subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


view : Model -> Html Msg
view model =
  div []
      [
        input  [ type_ "number", value "0", onInput SetMaxRounds] []
      , button [ onClick Start ]        [ text "Start" ]
      , drawRound(model.current)
      ]
