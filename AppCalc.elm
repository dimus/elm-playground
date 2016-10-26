module AppCalc exposing (..)

import Mouse
import AppTypes exposing (..)


distance : Mouse.Position -> Landmark -> Int
distance pos lm =
    let
        lmPos =
            getRealPos lm

        x =
            (pos.x - lmPos.x) ^ 2 |> toFloat

        y =
            (pos.y - lmPos.y) ^ 2 |> toFloat
    in
        x + y |> sqrt |> floor


getRealPos : Landmark -> Mouse.Position
getRealPos { pos, label, drag } =
    case drag of
        Nothing ->
            pos

        Just drag ->
            Mouse.Position
                (pos.x - drag.start.x + drag.current.x)
                (pos.y - drag.start.y + drag.current.y)
