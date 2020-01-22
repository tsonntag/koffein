module React exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Task
import Time exposing (..)
import TimeStamp exposing (..)
import Point exposing (..)
import Debug exposing (log)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model = { showAfter: Float
                   , createdAt: Maybe Time.Posix
                   , clickedAt: Maybe Time.Posix
                   , clicked:   Bool
                   , now:       Maybe Time.Posix
                   , point:     Maybe Point
                   , timeZone:  Maybe Time.Zone
                   }


newModel : Float -> Maybe Point -> Model
newModel showAfter point =
    { showAfter = showAfter
    , clicked = False
    , createdAt = Nothing
    , clickedAt = Nothing
    , point = point
    , now = Nothing
    , timeZone = Nothing
    }

initialModel : Model
initialModel  = newModel 0 Nothing

randomModelCmd : ((Float, (Int, Int)) -> Msg) -> Cmd Msg
randomModelCmd msg =
    let
        point =
            Random.pair (Random.int 0 10) (Random.int 0 20)

        showAfter =
            Random.float 500 2000
    in
        Random.generate (log "RANDOM MODEL CMD" msg) (Random.pair showAfter point)

boolToString : Bool -> String
boolToString   bool =
               if bool then "true" else "false"

drawModel : Model -> Html Msg
drawModel model = div []
                  [ h1 [] [ text "Model" ]
                  , p  [] [ text "Now: "
                          , text (TimeStamp.toString model.timeZone model.now)
                          ]
                  , p  [] [ text "Point show after: "
                          , text (String.fromFloat  model.showAfter)
                          ]
                  , p  [] [ text "Created: "
                          , text (TimeStamp.toString model.timeZone model.createdAt)
                          ]
                  , p  [] [ text "Clicked: "
                          , text (TimeStamp.toString model.timeZone model.clickedAt)
                          ]
                  , p  [] [ text "Point: "
                          , drawPoint model.point
                          ]
                  ]


nowCmd = Task.perform SetNow Time.now

getTime : (Time.Posix -> Msg) -> Cmd Msg
getTime msg =
    Task.perform (log "GET TIME" msg) Time.now


type Msg
  = CreateRandomModel
  | Clicked
  | SetClickedAt  Time.Posix
  | SetCreatedAt  Time.Posix
  | SetNow        Time.Posix
  | SetTimeZone   Time.Zone
  | CreateModel (Float, (Int, Int))


init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel |> log "INIT INITIAL MODEL"
  , randomModelCmd CreateModel |> log "INIT RANDOM"
  )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CreateRandomModel ->
      ( model
      , randomModelCmd CreateModel
      )

    Clicked ->
        ({ model | clicked = True }
        , getTime SetClickedAt
        )

    SetNow time ->
        ({ model | now = Just time }
        , Cmd.none
        )

    SetTimeZone zone ->
        ({ model | timeZone = Just zone }
        , Cmd.none
        )


    SetClickedAt time ->
        ({ model | clickedAt = Just time, now = Just time  }
             |> log "SET CLICKED AT"
        , Cmd.none
        )

    SetCreatedAt time ->
        ({ model | createdAt = Just time, now = Just time }
             |> log "SET CREATED AT"

        , Cmd.none
        )

    CreateModel (showAfter, (x, y)) ->
        ( newModel showAfter (Just (newPoint x y))
             |> log "SET CREATED AT"
        , Cmd.batch
              [ getTime SetCreatedAt
              , Task.perform SetTimeZone Time.here]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  div []
      [ drawModel(model)
      , button [ onClick CreateRandomModel  ] [ text "Create" ]
      , button [ onClick Clicked ] [ text "Click" ]
      ]

