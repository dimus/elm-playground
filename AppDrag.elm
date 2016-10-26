module AppDrag exposing (dragStart, dragEnd, dragUpdate)

import Mouse
import AppTypes exposing (..)
import AppCalc exposing (..)


dragStart : Model -> Label -> Mouse.Position -> ( Model, Cmd Msg )
dragStart model label pos =
    let
        drag =
            Just <| Drag pos pos
    in
        ( (List.map (addDragStatus label drag) model), Cmd.none )


dragUpdate : Model -> Label -> Mouse.Position -> ( Model, Cmd Msg )
dragUpdate model label pos =
    ( (List.map (updateDragStatus label pos) model), Cmd.none )


dragEnd : Model -> Label -> Mouse.Position -> ( Model, Cmd Msg )
dragEnd model label pos =
    ( (List.map (removeDragStatus label) model), Cmd.none )


removeDragStatus : Label -> Landmark -> Landmark
removeDragStatus label landmark =
    let
        pos =
            getRealPos landmark
    in
        { landmark | pos = pos, drag = Nothing }


updateDragStatus : Label -> Mouse.Position -> Landmark -> Landmark
updateDragStatus label pos landmark =
    let
        drag =
            landmark.drag
    in
        if landmark.label == label then
            { landmark | drag = Maybe.map (\{ start } -> Drag start pos) drag }
        else
            landmark


addDragStatus : Label -> Maybe Drag -> Landmark -> Landmark
addDragStatus label drag landmark =
    if landmark.label == label then
        { landmark | drag = drag }
    else
        landmark
