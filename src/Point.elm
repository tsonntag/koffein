module Point exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)


type alias Point = { x: Int, y: Int, visible: Bool }

newPoint : Int -> Int -> Point
newPoint x y =
    { x = x, y = y, visible = True}

drawPoint : Maybe Point -> Html msg
drawPoint point =
    case point of
        Just p ->
            div [] [ text "x="
                   , text (String.fromInt p.x)
                   , text "y="
                   , text (String.fromInt p.y)
                   ]
        Nothing ->
            div [] []

