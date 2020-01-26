module Farben exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)
import Debug exposing (log)
import Utils exposing (..)
import ItemLists exposing (..)

main =
  Browser.element
    { init = init , update = update , subscriptions = subscriptions , view = view}

type alias Item = ( String, String )
type alias Model = { items : List Item
                   , inputInterval : String
                   , interval : Int
                   , inputFactor : String
                   , factor : Float
                   , item : Maybe  Item
                   }


itemLists = ItemLists.data

initialModel : Model
initialModel =
    { items = []
    , item = Nothing
    , inputInterval = "1500"
    , interval = 1500
    , inputFactor = "0.95"
    , factor = 0.95
    }


type Msg
  = Start
  | Next
  | SetInputInterval String
  | SetInputFactor String


nextCmd : Model -> Cmd Msg
nextCmd model = after model.interval Next

init : () -> (Model, Cmd Msg)
init _ = ( initialModel, Cmd.none )


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Start ->
            let
                factor   = model.inputFactor   |> String.toFloat |> Maybe.withDefault model.factor
                interval = model.inputInterval |> String.toInt   |> Maybe.withDefault model.interval
            in

            (
             { initialModel |
                   factor = factor,
                   inputFactor = factor |> String.fromFloat,
                   interval = interval,
                   inputInterval = interval |> String.fromInt
             }
            , after 500 Next )
            --, nextCmd initialModel )

        Next ->
            case model.items of
                [] ->
                    ( model, Cmd.none )

                item :: rest ->
                    ( { model | interval = (toFloat model.interval) * model.factor |> ceiling,
                                item = Just item,
                                items = rest }
                    , nextCmd model
                    )

        SetInputFactor val ->
            ( { model | inputFactor = val } , Cmd.none )

        SetInputInterval val ->

            ( { model | inputInterval = val } , Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


viewItem : Maybe Item -> Html Msg
viewItem item =
    case item of
        Nothing ->
            text ""
        Just ( text_, color )  ->
            div [ style "color" color]
                [ h1 [] [ text text_ ] ]

view : Model -> Html Msg
view model =
    div [ class "container"]
        [ h1 [] [text "Welche Farbe hat das Wort ?"]
        , div [ class "row mt-5" ]
            [ div   [ style "width" "100px"] [ text "Faktor:"]
            , input [ onInput SetInputFactor
                    , value model.inputFactor
                    ] []
            ]
        , div [ class "row mt-5" ]
            [ div   [ style "width" "100px"] [ text "Intervall:"]
            , input [ onInput SetInputInterval
                    , value model.inputInterval
                    ] []
--          , text  ( String.fromInt model.interval )
            ]
        , div [ class "row mt-5" ]
            [ button [ onClick Start ] [ text "Start" ]
            ]
        , div [ class "row mt-5" ]
            [ viewItem model.item
            ]
        ]
