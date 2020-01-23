module Round exposing (..)

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


type alias Round = { showAfter: Maybe Int
                   , startedAt: Maybe Int
                   , clickedAt: Maybe Int
                   , point:     Maybe Point
                   , show:      Bool
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
  = Start
  | ShowPoint
  | SetClickedAt  Time.Posix
  | SetStartedAt  Time.Posix
  | SetShowAfter  Int
  | SetPoint      (Int, Int)
  | PointMsg      Point.Msg


init : () -> (Round, Cmd Msg)
init _ = ( initialRound, Cmd.none )


update: Msg -> Round -> (Round, Cmd Msg)
update msg round =
  case msg of
    Start ->
        ( initialRound
        , startCmd
        )

    SetShowAfter showAfter ->
        ( { round | showAfter = Just showAfter }
        , after showAfter ShowPoint )

    SetClickedAt time ->
        ({ round | clickedAt = Just <| Time.posixToMillis time } , Cmd.none )

    SetStartedAt time ->
        ( { round | startedAt = Just <| Time.posixToMillis time }, Cmd.none)

    SetPoint (x, y) ->
        ( { round | point = Just (Point x y) }, Cmd.none )

    ShowPoint ->
        ( { round | show = True }, Cmd.none )

    PointMsg Point.Clicked ->
        ( round , Task.perform SetClickedAt Time.now)


view : Round -> Html Msg
view round =
  div []
      [ button [ onClick Start ]   [ text "Start Round" ]
      , drawRound(round)
      ]
