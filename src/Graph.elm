module Graph exposing (..)

import Array exposing (Array)
import Graph.Links as Links exposing (Link, Links, Position)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import IdDict exposing (IdDict)
import Node exposing (Node(..), NumberNode(..), State(..), error, result)
import Svg exposing (Svg, foreignObject, path, svg, text_)
import Svg.Attributes exposing (class, d, dominantBaseline, fill, height, stroke, strokeWidth, textAnchor, width, x, y)
import UUID exposing (UUID)


columnWidth : Float
columnWidth =
    250


horizontalGap : Float
horizontalGap =
    100


verticalGap : Float
verticalGap =
    40


type alias Graph =
    Node


type alias Actions msg =
    { run : msg
    }


view : ( Float, Float ) -> Graph -> Actions msg -> Html msg
view ( width, height ) graph actions =
    let
        metadata =
            buildMetadata ( width, height ) graph
    in
    svg [ Svg.Attributes.width <| String.fromFloat width, Svg.Attributes.height <| String.fromFloat height ] <|
        List.concat
            [ viewLinks metadata.links
            , viewNode metadata graph
            , controls actions
            ]


controls : Actions msg -> List (Svg msg)
controls actions =
    [ foreignObject [ class "w-20 h-10" ]
        [ div []
            [ button [ onClick actions.run ] [ text "Run" ]
            ]
        ]
    ]


viewNode : DisplayGraph -> Node -> List (Svg msg)
viewNode metadata node =
    let
        renderNode : Position -> Size -> Svg msg
        renderNode position size =
            foreignObject
                [ x <| String.fromFloat position.x
                , y <| String.fromFloat position.y
                , width <| String.fromFloat size.width
                , height <| String.fromFloat size.height
                ]
                [ Node.view node ]

        render : UUID -> Maybe String -> Maybe String -> List (Svg msg)
        render id maybeError maybeValue =
            case IdDict.get id metadata.nodes of
                Nothing ->
                    []

                Just ( position, size ) ->
                    [ renderNode position size
                    , Maybe.map
                        (\value ->
                            text_
                                [ x <| String.fromFloat <| position.x - 5
                                , y <| String.fromFloat <| position.y + size.height / 2
                                , width <| String.fromFloat size.width
                                , height <| String.fromFloat size.height
                                , textAnchor "end"
                                , dominantBaseline "middle"
                                ]
                                [ text value ]
                        )
                        maybeValue
                        |> Maybe.withDefault (text "")
                    , Maybe.map
                        (\error ->
                            text_
                                [ x <| String.fromFloat <| position.x
                                , y <| String.fromFloat <| position.y + size.height + 5
                                , width <| String.fromFloat size.width
                                , height <| String.fromFloat size.height
                                , textAnchor "start"
                                , dominantBaseline "hanging"
                                ]
                                [ text error ]
                        )
                        maybeError
                        |> Maybe.withDefault (text "")
                    ]
    in
    case node of
        NumberNode (NumberConstant { id, state } _) ->
            render id (error state) (result String.fromFloat state)

        NumberNode (NumberAddition { id, state } (Just left) (Just right)) ->
            List.concat
                [ render id (error state) (result String.fromFloat state)
                , viewNode metadata (NumberNode left)
                , viewNode metadata (NumberNode right)
                ]

        NumberNode (NumberAddition { id, state } (Just left) Nothing) ->
            List.concat
                [ render id (error state) (result String.fromFloat state)
                , viewNode metadata (NumberNode left)
                ]

        NumberNode (NumberAddition { id, state } Nothing (Just right)) ->
            List.concat
                [ render id (error state) (result String.fromFloat state)
                , viewNode metadata (NumberNode right)
                ]

        NumberNode (NumberAddition { id, state } Nothing Nothing) ->
            render id (error state) (result String.fromFloat state)


viewLinks : Links -> List (Svg msg)
viewLinks =
    Links.mapToList
        (\link ->
            path
                [ d <|
                    "M"
                        ++ String.fromFloat link.output.x
                        ++ " "
                        ++ String.fromFloat link.output.y
                        ++ " C"
                        ++ (String.fromFloat <| link.output.x + verticalGap * 4)
                        ++ " "
                        ++ String.fromFloat link.output.y
                        ++ ", "
                        ++ (String.fromFloat <| link.input.x - verticalGap * 4)
                        ++ " "
                        ++ String.fromFloat link.input.y
                        ++ ", "
                        ++ String.fromFloat link.input.x
                        ++ " "
                        ++ String.fromFloat link.input.y
                , stroke "black"
                , strokeWidth "2"
                , fill "transparent"
                ]
                []
        )


type alias Size =
    { width : Float
    , height : Float
    }


type alias Metadata =
    { size : Size
    , nodes :
        Array
            { total : Int
            , placed : Int
            , height : Float
            , occupied : Float
            }
    , links : Links
    }


type alias DisplayGraph =
    { nodes : IdDict ( Position, Size )
    , links : Links
    }


buildMetadata : ( Float, Float ) -> Graph -> DisplayGraph
buildMetadata ( width, height ) graph =
    let
        buildNodesMetadata : Node -> Int -> Metadata -> Metadata
        buildNodesMetadata node depth metadata =
            let
                updatedMetadata =
                    { metadata
                        | nodes =
                            if Array.length metadata.nodes <= depth then
                                Array.push { total = 1, placed = 0, height = Node.height node, occupied = 0 } metadata.nodes

                            else
                                Array.set depth
                                    (Array.get depth metadata.nodes
                                        |> Maybe.map (\count -> { count | total = count.total + 1, height = count.height + Node.height node + verticalGap })
                                        |> Maybe.withDefault { total = 1, placed = 0, height = Node.height node, occupied = 0 }
                                    )
                                    metadata.nodes
                    }
            in
            case node of
                NumberNode (NumberConstant _ _) ->
                    updatedMetadata

                NumberNode (NumberAddition _ (Just nodeA) (Just nodeB)) ->
                    updatedMetadata
                        |> buildNodesMetadata (NumberNode nodeA) (depth + 1)
                        |> buildNodesMetadata (NumberNode nodeB) (depth + 1)

                NumberNode (NumberAddition _ (Just nodeA) Nothing) ->
                    buildNodesMetadata (NumberNode nodeA) (depth + 1) updatedMetadata

                NumberNode (NumberAddition _ Nothing (Just nodeB)) ->
                    buildNodesMetadata (NumberNode nodeB) (depth + 1) updatedMetadata

                NumberNode (NumberAddition _ Nothing Nothing) ->
                    updatedMetadata

        buildLinksMetadata : Maybe ( UUID, Position ) -> Node -> Int -> Metadata -> Metadata
        buildLinksMetadata maybeParentOutput node depth metadata =
            let
                position =
                    Position
                        (((toFloat depth + 1) * columnWidth + (toFloat depth - 1) * horizontalGap)
                            + ((width / 2) - ((toFloat <| Array.length metadata.nodes) * (columnWidth + horizontalGap) - horizontalGap) / 2)
                        )
                        ((Node.height node / 2) + (height / 2) - (Array.get depth metadata.nodes |> Maybe.map .height |> Maybe.withDefault 0) / 2)

                updateMetadata id =
                    case maybeParentOutput of
                        Nothing ->
                            metadata

                        Just ( parentId, parentOutputY ) ->
                            { metadata
                                | links = Links.insert ( parentId, id ) (Link parentOutputY position) metadata.links
                            }
            in
            case node of
                NumberNode (NumberConstant { id } _) ->
                    updateMetadata id

                NumberNode (NumberAddition { id } (Just left) (Just right)) ->
                    updateMetadata id
                        |> buildLinksMetadata (Just ( id, position )) (NumberNode left) (depth + 1)
                        |> buildLinksMetadata (Just ( id, position )) (NumberNode right) (depth + 1)

                NumberNode (NumberAddition { id } (Just left) Nothing) ->
                    updateMetadata id
                        |> buildLinksMetadata (Just ( id, position )) (NumberNode left) (depth + 1)

                NumberNode (NumberAddition { id } Nothing (Just right)) ->
                    updateMetadata id
                        |> buildLinksMetadata (Just ( id, position )) (NumberNode right) (depth + 1)

                NumberNode (NumberAddition { id } Nothing Nothing) ->
                    updateMetadata id

        toDisplayGraph : Node -> Int -> Metadata -> DisplayGraph -> ( DisplayGraph, Metadata )
        toDisplayGraph node depth metadata display =
            let
                newNode =
                    ( Position
                        ((metadata.size.width / 2) - ((toFloat <| Array.length metadata.nodes) * (columnWidth + horizontalGap) - horizontalGap) / 2 + toFloat depth * (columnWidth + horizontalGap))
                        ((metadata.size.height / 2) - (Array.get depth metadata.nodes |> Maybe.map .height |> Maybe.withDefault 0) / 2 + (Array.get depth metadata.nodes |> Maybe.map .occupied |> Maybe.withDefault 0))
                    , Size
                        columnWidth
                        (Node.height node)
                    )

                updatedMetadata =
                    { metadata
                        | nodes =
                            metadata.nodes
                                |> Array.indexedMap
                                    (\index count ->
                                        if index == depth then
                                            { count
                                                | placed = count.placed + 1
                                                , occupied = count.occupied + Node.height node + verticalGap
                                            }

                                        else
                                            count
                                    )
                    }
            in
            case node of
                NumberNode (NumberConstant { id } _) ->
                    ( { display | nodes = IdDict.insert id newNode display.nodes }
                    , updatedMetadata
                    )

                NumberNode (NumberAddition { id } (Just left) (Just right)) ->
                    let
                        ( leftDisplay, leftMetadata ) =
                            toDisplayGraph (NumberNode left) (depth + 1) updatedMetadata display

                        ( rightDisplay, rightMetadata ) =
                            toDisplayGraph (NumberNode right) (depth + 1) leftMetadata leftDisplay
                    in
                    ( { rightDisplay | nodes = IdDict.insert id newNode rightDisplay.nodes }
                    , rightMetadata
                    )

                NumberNode (NumberAddition { id } (Just left) Nothing) ->
                    let
                        ( leftDisplay, leftMetadata ) =
                            toDisplayGraph (NumberNode left) (depth + 1) updatedMetadata display
                    in
                    ( { leftDisplay | nodes = IdDict.insert id newNode leftDisplay.nodes }
                    , leftMetadata
                    )

                NumberNode (NumberAddition { id } Nothing (Just right)) ->
                    let
                        ( rightDisplay, rightMetadata ) =
                            toDisplayGraph (NumberNode right) (depth + 1) updatedMetadata display
                    in
                    ( { rightDisplay | nodes = IdDict.insert id newNode rightDisplay.nodes }
                    , rightMetadata
                    )

                NumberNode (NumberAddition { id } Nothing Nothing) ->
                    ( { display | nodes = IdDict.insert id newNode display.nodes }
                    , updatedMetadata
                    )

        updateLinks : Maybe ( UUID, Position ) -> Node -> DisplayGraph -> DisplayGraph
        updateLinks maybeParent node metadata =
            let
                output : UUID -> Int -> Int -> Maybe ( UUID, Position )
                output id count placement =
                    Maybe.map
                        (\( position, size ) ->
                            let
                                lineHeight =
                                    size.height / (toFloat count + 1)
                            in
                            ( id
                            , { x = position.x + size.width
                              , y = position.y + lineHeight * toFloat placement
                              }
                            )
                        )
                        (IdDict.get id metadata.nodes)

                link : UUID -> Links -> Links
                link id links =
                    Maybe.map2
                        (\( parentId, parentPosition ) ( position, size ) ->
                            Links.insert ( parentId, id ) (Link parentPosition { x = position.x, y = position.y + size.height / 2 }) links
                        )
                        maybeParent
                        (IdDict.get id metadata.nodes)
                        |> Maybe.withDefault links
            in
            case node of
                NumberNode (NumberConstant { id } _) ->
                    { metadata | links = link id metadata.links }

                NumberNode (NumberAddition { id } (Just left) (Just right)) ->
                    let
                        leftMetadata =
                            updateLinks (output id 2 1) (NumberNode left) metadata

                        rightMetadata =
                            updateLinks (output id 2 2) (NumberNode right) leftMetadata
                    in
                    { rightMetadata | links = link id rightMetadata.links }

                NumberNode (NumberAddition { id } (Just left) Nothing) ->
                    let
                        leftMetadata =
                            updateLinks (output id 2 1) (NumberNode left) metadata
                    in
                    { leftMetadata | links = link id leftMetadata.links }

                NumberNode (NumberAddition { id } Nothing (Just right)) ->
                    let
                        rightMetadata =
                            updateLinks (output id 2 2) (NumberNode right) metadata
                    in
                    { rightMetadata | links = link id rightMetadata.links }

                NumberNode (NumberAddition { id } Nothing Nothing) ->
                    { metadata | links = link id metadata.links }
    in
    buildNodesMetadata graph 0 { nodes = Array.empty, size = { width = width, height = height }, links = Links.empty }
        |> buildLinksMetadata Nothing graph 0
        |> (\metadata -> toDisplayGraph graph 0 metadata { nodes = IdDict.empty, links = metadata.links })
        |> Tuple.first
        |> updateLinks Nothing graph
