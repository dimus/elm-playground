module AppTypes exposing (..)

import Mouse


type alias Drag =
    { start : Mouse.Position, current : Mouse.Position }


type alias Model =
    List Landmark


type alias Landmark =
    { pos : Mouse.Position
    , label : String
    , drag : Maybe Drag
    }


type alias Label =
    String


type Msg
    = CreateLandmark Mouse.Position
    | Reset
    | DragStart Label Mouse.Position
    | DragAt Label Mouse.Position
    | DragEnd Label Mouse.Position
