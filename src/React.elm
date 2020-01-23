module React exposing (..)

import Browser
import Html exposing (..)
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


type alias Model = { showAfter: Maybe Int
                   , startedAt: Maybe Int
                   , clickedAt: Maybe Int
                   , point:     Maybe Point
                   , show:      Bool
                   }


initialModel : Model
initialModel =
    { showAfter = Nothing
    , startedAt = Nothing
    , clickedAt = Nothing
    , point     = Nothing
    , show      = False
    }



drawInt : Maybe Int -> String
drawInt int = int |> Maybe.map String.fromInt |> Maybe.withDefault ""


drawModel : Model -> Html Msg
drawModel model
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
              -- , p  [] [ text "Point: "
              --         , drawPointText model.point
              --      ]
              , div  [] [ case (model.show, model.point) of
                              (True, Just p) -> Html.map PointMsg (drawPoint p)
                              _ -> text ""
                        ]
              ]

duration : Model -> Maybe Int
duration  { clickedAt, startedAt, showAfter } =
    case (clickedAt, startedAt, showAfter) of
        (Just a, Just b, Just c) ->
            Just (a - b - c)
        _ -> Nothing



after : Int -> Msg -> Cmd Msg
after time msg =
    Process.sleep (toFloat time) |> Task.perform (always msg)

type Msg
  = Start
  | ShowPoint
  | SetClickedAt  Time.Posix
  | SetStartedAt  Time.Posix
  | SetShowAfter  Int
  | SetPoint      (Int, Int)
  | PointMsg      Point.Msg


init : () -> (Model, Cmd Msg)
init _ = ( initialModel, Cmd.none )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Start ->
        ( initialModel
        , Cmd.batch
              [ Task.perform    SetStartedAt Time.now
              , Random.generate SetPoint     (Random.pair (Random.int 0 10) (Random.int 0 20))
              , Random.generate SetShowAfter (Random.int 1000 5000)
              ]
        )

    SetShowAfter showAfter ->
        ( { model | showAfter = Just showAfter }
        , after showAfter ShowPoint )

    SetClickedAt time ->
        ({ model | clickedAt = Just <| Time.posixToMillis time }
        , Cmd.none )

    SetStartedAt time ->
        ( { model | startedAt = Just <| Time.posixToMillis time },
              Cmd.none)

    SetPoint (x, y) ->
        ( { model | point = Just (Point x y) }, Cmd.none )

    ShowPoint ->
        ( { model | show = True }, Cmd.none )


    PointMsg Point.Clicked ->
        ( model , Task.perform SetClickedAt Time.now)



subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


view : Model -> Html Msg
view model =
  div []
      [ button [ onClick Start ]   [ text "Start" ]
      , drawModel(model)
      ]
