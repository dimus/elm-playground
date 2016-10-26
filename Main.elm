module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=), object2, int, decodeString, map)
import Mouse exposing (position)
import String
import Svg.Attributes as SA exposing (..)
import Svg.Keyed as SK exposing (node)
import Svg
    exposing
        ( defs
        , linearGradient
        , style
        , stop
        , g
        , circle
        , rect
        , svg
        , text'
        , Svg
        )


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    List Landmark


type alias Landmark =
    { pos : Mouse.Position
    , label : String
    , drag : Maybe Drag
    }


type alias Drag =
    { start : Mouse.Position, current : Mouse.Position }


init : ( Model, Cmd Msg )
init =
    ( [], Cmd.none )



-- UPDATE


type Msg
    = CreateLandmark Mouse.Position
    | Reset
    | DragStart Label Mouse.Position
    | DragAt Label Mouse.Position
    | DragEnd Label Mouse.Position


type alias Label =
    String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateLandmark pos ->
            if (List.length model >= 10 || pos.x > 400 || pos.y > 400) then
                ( model, Cmd.none )
            else
                ( (Landmark pos (label model) Nothing) :: model, Cmd.none )

        Reset ->
            init

        DragStart label pos ->
            let
                drag =
                    Just <| Drag pos pos
            in
                ( (List.map (startDrag label drag) model), Cmd.none )

        DragAt label pos ->
            ( (List.map (updateDrag label pos) model), Cmd.none )

        DragEnd label pos ->
            let
                drag =
                    Nothing
            in
                ( (List.map (stopDrag label) model), Cmd.none )


stopDrag label landmark =
    let
        pos =
            getRealPos landmark
    in
        { landmark | pos = pos, drag = Nothing }


updateDrag label pos landmark =
    let
        drag =
            landmark.drag
    in
        if landmark.label == label then
            { landmark | drag = Maybe.map (\{ start } -> Drag start pos) drag }
        else
            landmark


startDrag label drag landmark =
    if landmark.label == label then
        { landmark | drag = drag }
    else
        landmark



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        draggedLabel =
            draggedLandmark model
    in
        case draggedLabel of
            Nothing ->
                Sub.none

            Just l ->
                Sub.batch
                    [ Mouse.moves (DragAt l.label)
                    , Mouse.ups (DragEnd l.label)
                    ]


draggedLandmark : Model -> Maybe Landmark
draggedLandmark model =
    List.head <|
        List.filter (\landmark -> landmark.drag /= Nothing) model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ SK.node "svg"
            [ SA.width "400px"
            , SA.height "400px"
            , viewBox "0 0 400 400"
            , HA.style [ ( "border", "1px solid #eee" ) ]
            , onMouseClick
            ]
            (drawLandmarks model)
        , div [] [ text <| toString model ]
        , br [] []
        , slider model
        , button [ onClick Reset ] [ text "Reset" ]
        , table [] (rows model)
        ]


drawLandmarks model =
    List.map makeLandmark <| model


slider : Model -> Svg.Svg Msg
slider model =
    let
        length =
            400

        percent =
            List.length model * (length // 10)
    in
        Svg.svg
            [ SA.width <| toString (length + 100) ++ "px"
            , SA.height "40px"
            , viewBox "-10 -5 500 40"
            ]
            [ defs []
                [ linearGradient
                    [ SA.id "darkFaderGradient"
                    , gradientUnits "objectBoundingBox"
                    , x2 "0"
                    , y2 "1"
                    ]
                    [ stop [ offset "0", stopColor "#D7D9DC" ] []
                    , stop [ offset "1", stopColor "#42494C" ] []
                    ]
                , linearGradient
                    [ SA.id "lightFaderGradient"
                    , gradientUnits "objectBoundingBox"
                    , x2 "0"
                    , y2 "1"
                    ]
                    [ stop [ offset "0", stopColor "white" ] []
                    , stop [ offset "1", stopColor "#D7D9DC" ] []
                    ]
                ]
            , g []
                [ g [ SA.id "fader", SA.style "stroke:#989B9F" ]
                    [ rect
                        [ SA.class "faderBackground"
                        , SA.id "svg01"
                        , rx "10"
                        , SA.height "20"
                        , SA.width (toString length)
                        , SA.style """fill:url(#lightFaderGradient)"""
                        ]
                        []
                    , rect
                        [ SA.class "faderForeground"
                        , SA.id "svg02"
                        , rx "10"
                        , SA.height "20"
                        , SA.width (toString percent)
                        , SA.style """fill:url(#darkFaderGradient)"""
                        ]
                        []
                    ]
                ]
            ]


label : Model -> String
label model =
    (String.cons 'd') <| toString ((List.length model) + 1)


makeLandmark : Landmark -> ( Label, Svg Msg )
makeLandmark lm =
    let
        realPos =
            getRealPos lm

        x =
            toString realPos.x

        y =
            toString realPos.y

        yl =
            toString (realPos.y - 10) ++ "px"

        l =
            lm.label

        svgLandmark =
            g []
                [ text'
                    [ SA.x x
                    , SA.y yl
                    , fontFamily "sans-serif"
                    , fontSize "10px"
                    , textAnchor "middle"
                    ]
                    [ Svg.text l ]
                , circle
                    [ cx x
                    , cy y
                    , r "5"
                    , stroke "#0B79CE"
                    , onMouseDown l
                    ]
                    []
                ]
    in
        ( l, svgLandmark )


getRealPos : Landmark -> Mouse.Position
getRealPos { pos, label, drag } =
    case drag of
        Nothing ->
            pos

        Just drag ->
            Mouse.Position
                (pos.x - drag.start.x + drag.current.x)
                (pos.y - drag.start.y + drag.current.y)


rows model =
    header :: List.reverse (List.map row model)


row model =
    tr []
        [ td [] [ text model.label ]
        , td [] [ text (toString model.pos.x) ]
        , td [] [ text (toString model.pos.y) ]
        ]


header =
    tr []
        [ th [] [ text "Label" ]
        , th [] [ text "X" ]
        , th [] [ text "Y" ]
        ]


onMouseClick =
    on "dblclick" (Json.map CreateLandmark Mouse.position)


onMouseDown label =
    on "mousedown" (Json.map (DragStart label) Mouse.position)
