module Farben exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)
import Debug exposing (log)
import Utils exposing (..)
import ItemLists exposing (data)
import Array exposing (..)

main =
  Browser.element
    { init = init , update = update , subscriptions = subscriptions , view = view}

type alias Item = ( String, String )
type alias ItemList = List Item
type alias Model = { inputInterval : String
                   , interval      : Int
                   , inputFactor   : String
                   , factor        : Float
                   , item          : Maybe Item
                   , itemListIndex : Maybe Int
                   , itemList      : Maybe ItemList
                   }

itemLists : Array ItemList
itemLists = ItemLists.data |> Array.fromList

nItemLists : Int
nItemLists = Array.length itemLists

initialItemListIndex : Maybe Int
initialItemListIndex = if Array.isEmpty itemLists then Nothing else Just 0

initialModel : Model
initialModel =
    { itemListIndex = initialItemListIndex
    , itemList      = getItemList initialItemListIndex
    , item          = Nothing
    , inputInterval = "1500"
    , interval      = 1500
    , inputFactor   = "0.95"
    , factor        = 0.95
    }

resetModel : Maybe Int -> Model -> Model
resetModel itemListIndex model =
    { model |
      itemListIndex = itemListIndex
    , itemList      = getItemList itemListIndex
    , item          = Nothing
    }

type Msg
  = Start
  | NextItem
  | SetItemList String
  | SetInputInterval String
  | SetInputFactor String

spy : String -> a -> a
spy info thing =
    Debug.log (info ++ (Debug.toString thing)) thing


nextCmd : Model -> Cmd Msg
nextCmd model = after model.interval NextItem


getItemList : Maybe Int -> Maybe ItemList
getItemList index =
    case index of
        Just i ->
            Array.get i itemLists
        _ ->
            Nothing


init : () -> (Model, Cmd Msg)
init _ = ( initialModel, Cmd.none )


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Start ->
            let
                factor   = model.inputFactor   |> String.toFloat |> Maybe.withDefault model.factor    |> spy "FACTOR:"
                interval = model.inputInterval |> String.toInt   |> Maybe.withDefault model.interval  |> spy "INTERVAL:"
            in
                (
                 { model |
                   factor        = factor
                 , inputFactor   = factor |> String.fromFloat
                 , interval      = interval
                 , inputInterval = interval |> String.fromInt
                 , item          = model.itemListIndex |> getItemList |> Maybe.andThen List.head
                 }
                 |> resetModel model.itemListIndex

                , after 500 NextItem )

        NextItem ->
            case model.itemList of
                Just (item :: rest) ->
                    ( { model | interval = (toFloat model.interval) * model.factor |> ceiling,
                                item     = Just item,
                                itemList = Just rest }
                    , nextCmd model
                    )

                _ ->
                    ( model, Cmd.none )


        SetInputFactor val ->
            ( { model | inputFactor = val } , Cmd.none )

        SetInputInterval val ->
            ( { model | inputInterval = val } , Cmd.none )


        SetItemList val ->
            ( resetModel (String.toInt val) model, Cmd.none )

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

intToOption : Int ->Int -> Html Msg
intToOption selectedIndex i =
    option [ value (String.fromInt i)
           , selected (i == selectedIndex)
           ]
           [ text  (String.fromInt (i + 1))]


view : Model -> Html Msg
view model =
    div [ class "container"]
        [ h1 [] [text "Welche Farbe hat das Wort ?"]
        , (case model.itemListIndex of
               Just i ->
                  viewBody model i

               _ ->
                  div [] [ text "Keine Daten" ])
        ]

viewBody : Model -> Int -> Html Msg
viewBody model index =
    div []
        [ div [ class "row mt-5" ]
            [ div   [ style "width" "100px"] [ text "Faktor:"]
            , input [ onInput SetInputFactor
                    , value model.inputFactor
                    ] []
            ]
        , div [ class "row mt-2" ]
            [ div   [ style "width" "100px"] [ text "Intervall:"]
            , input [ onInput SetInputInterval
                    , value model.inputInterval
                    ] []
--          , text (String.fromInt model.interval)
            ]
        , div [ class "row mt-2" ]
            [ div    [ style "width" "100px"] [ text "Liste:"]
            , select [ onInput SetItemList]
                (List.map (intToOption index) (List.range 0 (nItemLists - 1)))
            ]

        , div [ class "row mt-5" ]
            [ button [ onClick Start ] [ text "Start" ]
            ]
        , div [ class "row mt-5" ]
            [ viewItem model.item
            ]
        ]
