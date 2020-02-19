module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Hexagons.Hex exposing (Hex)
import Hexagons.Layout as HexLayout exposing (Layout, Point)
import Hexagons.Map as HexMap exposing (Hash, Map)
import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Lazy


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias CellScore =
    Int


type alias Model =
    { cells : Map
    , markedCells : Dict Hash CellScore
    }


emptyModel : Model
emptyModel =
    { cells = HexMap.rectangularPointyTopMap 15 68
    , markedCells = Dict.empty
    }


init : Model
init =
    emptyModel


type Msg
    = NoOp
    | MarkCell Hash


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        MarkCell cell ->
            { model | markedCells = markCell cell model.markedCells }


markCell : Hash -> Dict Hash CellScore -> Dict Hash CellScore
markCell cell mcs =
    case Dict.get cell mcs of
        Nothing ->
            Dict.insert cell 1 mcs

        Just 3 ->
            Dict.remove cell mcs

        Just n ->
            Dict.insert cell (n + 1) mcs


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
                , Svg.Events.onClick <| MarkCell hexLocation
                ]
                []
            ]
    in
    Svg.g [] <|
        List.map2 toSvg
            (List.map getCellKey (Dict.toList model.cells))
            (List.map (pointsToString << mapPolygonCorners << getCell) (Dict.toList model.cells))


hexColor : Hash -> Model -> String
hexColor hexLocation model =
    case Dict.get hexLocation model.markedCells of
        Just 1 ->
            "#81a684"

        Just 2 ->
            "#57886c"

        Just 3 ->
            "#466060"

        _ ->
            "#e9ecef"


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
