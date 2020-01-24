module Main exposing (..)

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


type alias Model = { showAfter: Maybe Int
                   , startedAt: Maybe Int
                   , clickedAt: Maybe Int
                   , point:     Maybe Point
                   , showPoint: Bool
                   , maxRounds: Int
                   , round:     Int
                   , rounds:    List ( Int, Int )
                   , avg:       Maybe Int
                   }

initialModel : Model
initialModel =
    { showAfter = Nothing
    , startedAt = Nothing
    , clickedAt = Nothing
    , point     = Nothing
    , showPoint = False
    , maxRounds = 1
    , round     = 1
    , rounds    = []
    , avg       = Nothing
    }


drawInt : Maybe Int -> String
drawInt int = int |> Maybe.map String.fromInt |> Maybe.withDefault ""


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
  | SetMaxModels  String
  | SetClickedAt  Time.Posix
  | SetStartedAt  Time.Posix
  | SetShowAfter  Int
  | SetPoint      (Int, Int)
  | ShowPoint
  | PointMsg      Point.Msg


init : () -> (Model, Cmd Msg)
init _ = ( initialModel, Cmd.none )


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Start ->
        ( initialModel
        , startCmd )

    SetShowAfter showAfter ->
        ({ model | showAfter = Just showAfter }
        , after showAfter ShowPoint )

    SetMaxModels ns ->
        ({ model | maxRounds = ns |> String.toInt |> Maybe.withDefault 0 }
        , Cmd.none )

    SetClickedAt time ->
        ({ model | clickedAt = Just <| Time.posixToMillis time }
        , Cmd.none )

    SetStartedAt time ->
        ({ model | startedAt = Just <| Time.posixToMillis time }
        , Cmd.none)

    SetPoint (x, y) ->
        ({ model | point = Just (Point x y)}
        , Cmd.none )

    ShowPoint ->
        ({ model | showPoint = True }
        , Cmd.none )

    PointMsg Point.Clicked ->
        ( model
        , Task.perform SetClickedAt Time.now)


divClass : String -> List (Html Msg) -> Html Msg
divClass  c content =
    div [ class c ] content


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


col : List (Html Msg) -> Html Msg
col  content = divClass "col" content

row : List (Html Msg) -> Html Msg
row  content = divClass "row" content

container : List (Html Msg) -> Html Msg
container  content = divClass "container" content


view : Model -> Html Msg
view model =
    div [ class "container"]
        [ h1 [] [text "Messe Deine Reaktion"]
        , div [ class "row mt-5" ]
            [ div [ class "col" ]
                  [ button [ onClick Start ] [ text "Start" ]
                  ]
            ]
        , div [ class "row mt-2" ]
            [ div [ class "col-8"]
                  [ viewProperties model ]
            , col [ viewResult model ]
            ]
        , div [ class "row" ]
              [ viewField model
              ]
        ]


propertiesTable : List (Html Msg, Html Msg) -> Html Msg
propertiesTable props =
    table []
        (List.map (\( key, val) ->
                       tr []
                       [ td [] [ key ]
                       , td [] [ val ]
                       ]
                  )
             props)

viewProperties : Model -> Html Msg
viewProperties model =
    propertiesTable
          [ ( text "Runden:"
            , input [ value (String.fromInt model.maxRounds), onInput SetMaxModels ] []
            )

          , ( text "Runde: "
            , text <| String.fromInt model.round
            )

          , ( text "Start: "
            , text <| drawInt model.startedAt)

          , ( text "Zeige Punkt nach msecs: "
            , text <| drawInt model.showAfter)

          , ( text "Click: "
            , text <| drawInt model.clickedAt)

          , ( text "Reaktionszeit (msecs): "
            , text <| drawInt (duration model))
            ]


viewResult : Model -> Html Msg
viewResult model =
    propertiesTable [ (text "A", text "AAA")
                    , (text "B", text "BBB")
                    ]

viewField : Model -> Html Msg
viewField model =
    div [] [
             case (model.showPoint, model.point) of
                 (True, Just p) -> Html.map PointMsg (drawPoint p)
                 _ -> text ""
           ]
