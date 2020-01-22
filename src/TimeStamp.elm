module TimeStamp exposing (..)

import Time exposing (Zone, Posix)


toString : Maybe Zone -> Maybe Posix -> String
toString zone  time =
    case (zone |> log "ZONE", time |> log "TIME") of
        (Just z, Just t) ->
            let
                hour   = String.fromInt (Time.toHour   z t)
                minute = String.fromInt (Time.toMinute z t)
                second = String.fromInt (Time.toSecond z t)
            in
                hour ++ ":" ++ minute ++ ":" ++ second
        _ -> "-"
