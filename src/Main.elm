module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Hexagons.Hex exposing (Hex)
import Hexagons.Layout as HexLayout exposing (Layout, Point)
import Hexagons.Map as HexMap exposing (Hash, Map)
import Html exposing (Html)
import Maybe
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Lazy
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias CellScore =
    Int


type alias Model =
    { cells : Map
    , markedCells : Dict Hash CellScore
    , watTime : Time.Posix
    }


emptyModel : Model
emptyModel =
    { cells = HexMap.rectangularPointyTopMap 15 68
    , markedCells = Dict.empty
    , watTime = Time.millisToPosix 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( emptyModel, Task.perform (\_ -> NoOp) Time.now )


type Msg
    = NoOp
    | Tick Time.Posix
    | MarkCell Hash


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick _ ->
            ( { model | markedCells = evolveCells model.markedCells }, Cmd.none )

        MarkCell cell ->
            ( { model | markedCells = markCell cell model.markedCells }, Cmd.none )


cellWidth : Float
cellWidth =
    16.0


cellHeight : Float
cellHeight =
    16.0


svgWidth : Int
svgWidth =
    1920


svgHeight : Int
svgHeight =
    400


layout : Layout
layout =
    { orientation = HexLayout.orientationLayoutPointy
    , size = ( cellWidth, cellHeight )
    , origin = ( 0.0, 0.0 )
    }


viewBoxStringCoords : String
viewBoxStringCoords =
    String.fromFloat (-cellWidth + cellWidth * 0.1)
        ++ " "
        ++ String.fromFloat -(cellHeight + 0)
        ++ " "
        ++ String.fromInt svgWidth
        ++ " "
        ++ String.fromInt svgHeight


view : Model -> Html Msg
view model =
    Svg.svg
        [ Svg.Attributes.version "1.1"
        , Svg.Attributes.x "0"
        , Svg.Attributes.y "0"
        , Svg.Attributes.height (String.fromInt svgHeight)
        , Svg.Attributes.width (String.fromInt svgWidth)
        , Svg.Attributes.viewBox viewBoxStringCoords
        ]
        [ Svg.Lazy.lazy hexGrid model
        ]


hexGrid : Model -> Html Msg
hexGrid model =
    let
        toSvg : Hash -> String -> Svg Msg
        toSvg hexLocation cornersCoords =
            Svg.g [] (toPolygon hexLocation cornersCoords)

        toPolygon : Hash -> String -> List (Svg Msg)
        toPolygon hexLocation cornersCoords =
            [ Svg.polygon
                [ Svg.Attributes.style "cursor: arrow"
                , Svg.Attributes.stroke "#ffffff"
                , Svg.Attributes.strokeWidth "1px"
                , Svg.Attributes.fill <| hexColor hexLocation model
                , Svg.Attributes.points cornersCoords
                , Svg.Events.onMouseOver <| MarkCell hexLocation
                ]
                []
            ]
    in
    Svg.g [] <|
        List.map2 toSvg
            (List.map getCellKey (Dict.toList model.cells))
            (List.map (pointsToString << mapPolygonCorners << getCell) (Dict.toList model.cells))


evolveCells : Dict Hash CellScore -> Dict Hash CellScore
evolveCells =
    Dict.map (\_ n -> max 0 (n - 1))
        >> Dict.filter (\_ v -> v /= 0)


markCell : Hash -> Dict Hash CellScore -> Dict Hash CellScore
markCell cell markedCells =
    Dict.insert cell (Array.length gradients) markedCells


hexColor : Hash -> Model -> String
hexColor hash model =
    let
        collatzColor n =
            modBy (Array.length gradients) (collatz n)
    in
    Dict.get hash model.markedCells
        |> Maybe.withDefault 0
        |> (\n -> Array.get (collatzColor n) gradients)
        |> Maybe.withDefault darkestGradient


{-| Helper to convert points to SVG string coordinates
-}
pointsToString : List Point -> String
pointsToString points =
    String.join " " (List.map pointToStringCoords points)


{-| Helper to convert points to SVG string coordinates
-}
pointToStringCoords : Point -> String
pointToStringCoords ( x, y ) =
    String.fromFloat x ++ "," ++ String.fromFloat y


getCell : ( Hash, Hex ) -> Hex
getCell ( _, hex ) =
    hex


getCellKey : ( Hash, Hex ) -> Hash
getCellKey ( key, _ ) =
    key


mapPolygonCorners : Hex -> List Point
mapPolygonCorners =
    HexLayout.polygonCorners layout


gradients : Array String
gradients =
    Array.fromList
        [ lightestGradient
        , "#e0e0e0"
        , "#dcdcdc"
        , "#d8d8d8"
        , "#d3d3d3"
        , "#d0d0d0"
        , "#c8c8c8"
        , "#c0c0c0"
        , "#bebebe"
        , "#b8b8b8"
        , "#b0b0b0"
        , "#a9a9a9"
        , "#a8a8a8"
        , "#a0a0a0"
        , "#989898"
        , "#909090"
        , "#888888"
        , "#808080"
        , "#787878"
        , "#707070"
        , "#696969"
        , "#686868"
        , "#606060"
        ]


lightestGradient : String
lightestGradient =
    "#e9ecef"


darkestGradient : String
darkestGradient =
    "#585858"


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick


collatz : Int -> Int
collatz n =
    if n == 0 then
        0

    else if modBy 2 n == 0 then
        n // 2

    else
        3 * n + 1
