module Point exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


type alias Point = { x: Int, y: Int, visible: Bool }

newPoint : Int -> Int -> Point
newPoint x y =
    { x = x, y = y, visible = True}

drawPointText : Maybe Point -> Html msg
drawPointText point =
    case point of
        Just p ->
            div [] [ text "x="
                   , text (String.fromInt p.x)
                   , text "y="
                   , text (String.fromInt p.y)
                   ]
        Nothing ->
            div [] []


drawPoint : Maybe Point -> Html msg
drawPoint point =
    case point of
        Just p ->
            div [ style "background" "red"
                , style "width" "10px"
                , style "height" "10px"
                , style "margin-left" ((String.fromInt (p.x * 10)) ++ "px")
                , style "margin-top" ((String.fromInt (p.y * 10)) ++ "px")
                ] [text ""]
        Nothing ->
            div [] []

