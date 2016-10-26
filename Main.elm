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
import AppTypes exposing (..)
import AppDrag exposing (..)
import AppCalc exposing (..)
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


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


init : ( Model, Cmd Msg )
init =
    ( [], Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateLandmark pos ->
            createLandmark model pos

        Reset ->
            init

        DragStart label pos ->
            dragStart model label pos

        DragAt label pos ->
            dragUpdate model label pos

        DragEnd label pos ->
            dragEnd model label pos


createLandmark : Model -> Mouse.Position -> ( Model, Cmd Msg )
createLandmark model pos =
    if List.length model >= 10 then
        ( model, Cmd.none )
    else
        let
            lm =
                Landmark pos (label model) Nothing
        in
            ( lm :: model, Cmd.none )



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
        , br [] []
        , slider model
        , button [ onClick Reset ] [ text "Reset" ]
        , table [] (makeTable model)
        ]


drawLandmarks model =
    List.map makeLandmark <| model


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


makeTable : Model -> List (Html Msg)
makeTable model =
    (tableHeader model) :: List.reverse (List.map (tableRow model) model)


tableRow : Model -> Landmark -> Html Msg
tableRow model lm =
    let
        distances =
            List.map (distance lm.pos) model

        tds =
            [ td [] [ text lm.label ]
            , td [] [ text (toString lm.pos.x) ]
            , td [] [ text (toString lm.pos.y) ]
            ]

        tdsDistances =
            List.map (\d -> td [] [ text <| toString d ]) distances
    in
        tr [] <|
            List.append
                tds
            <|
                List.reverse tdsDistances


tableHeader : Model -> Html Msg
tableHeader model =
    let
        ths =
            [ th [] [ text "Label" ]
            , th [] [ text "X" ]
            , th [] [ text "Y" ]
            ]
    in
        tr [] (List.append ths (thLms model))


thLms : Model -> List (Html Msg)
thLms model =
    let
        labels =
            List.map .label model |> List.reverse
    in
        List.map (\l -> th [] [ text l ]) labels


onMouseClick : Attribute Msg
onMouseClick =
    on "dblclick" (Json.map CreateLandmark Mouse.position)


onMouseDown : Label -> Attribute Msg
onMouseDown label =
    on "mousedown" (Json.map (DragStart label) Mouse.position)
