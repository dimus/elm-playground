module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json exposing ((:=), object2, int, decodeString, map)
import Mouse
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
    }


init : ( Model, Cmd Msg )
init =
    ( [], Cmd.none )



-- UPDATE


type Msg
    = MouseMsg Mouse.Position
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMsg pos ->
            if (List.length model >= 10 || pos.x > 400 || pos.y > 400) then
                ( model, Cmd.none )
            else
                ( (Landmark pos (label model)) :: model, Cmd.none )

        Reset ->
            init



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.clicks MouseMsg



-- VIEW


(=>) =
    (,)


view : Model -> Html Msg
view model =
    div []
        [ SK.node "svg"
            [ SA.width "400px"
            , SA.height "400px"
            , viewBox "0 0 400 400"
            , HA.style [ ( "border", "1px solid #eee" ) ]
            ]
            (drawLandmarks model)
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


makeLandmark lm =
    let
        x =
            toString lm.pos.x

        y =
            toString lm.pos.y

        yl =
            toString (lm.pos.y - 10) ++ "px"

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
                , circle [ cx x, cy y, r "5", stroke "#0B79CE" ] []
                ]
    in
        ( l, svgLandmark )


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
