module Graph exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Node exposing (Node(..), NumberNode(..))
import Svg exposing (foreignObject, svg)
import Svg.Attributes exposing (height, width, x, y)


columnWidth : Float
columnWidth =
    250


horizontalGap : Float
horizontalGap =
    100


verticalGap : Float
verticalGap =
    20


type alias Graph =
    Node


type alias Metadata =
    { size : { width : Float, height : Float }
    , itemCount :
        Array
            { total : Int
            , placed : Int
            , height : Float
            , occupied : Float
            }
    }


view : ( Float, Float ) -> Graph -> Html msg
view ( width, height ) graph =
    let
        metadata =
            buildMetadata ( width, height ) graph

        ( nodes, _ ) =
            viewNode 0 metadata graph
    in
    svg [ Svg.Attributes.width <| String.fromFloat width, Svg.Attributes.height <| String.fromFloat height ] nodes


viewNode : Int -> Metadata -> Node -> ( List (Html msg), Metadata )
viewNode depth metadata node =
    let
        renderedNode =
            foreignObject
                [ x <| String.fromFloat <| (metadata.size.width / 2) - ((toFloat <| Array.length metadata.itemCount) * (columnWidth + horizontalGap) - horizontalGap) / 2 + toFloat depth * (columnWidth + horizontalGap)
                , y <| String.fromFloat <| (metadata.size.height / 2) - (Array.get depth metadata.itemCount |> Maybe.map .height |> Maybe.withDefault 0) / 2 + (Array.get depth metadata.itemCount |> Maybe.map .occupied |> Maybe.withDefault 0) - Node.height node / 2
                , width <| String.fromFloat columnWidth
                , height <| String.fromFloat <| Node.height node
                ]
                [ Node.view node ]

        updatedMetadata =
            { metadata
                | itemCount =
                    metadata.itemCount
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
        NumberNode (NumberConstant _ _) ->
            ( [ renderedNode ]
            , updatedMetadata
            )

        NumberNode (NumberAddition _ (Just nodeA) (Just nodeB)) ->
            let
                ( left, leftMetadata ) =
                    viewNode (depth + 1) updatedMetadata (NumberNode nodeA)

                ( right, rightMetadata ) =
                    viewNode (depth + 1) leftMetadata (NumberNode nodeB)
            in
            ( renderedNode
                :: List.concat
                    [ left
                    , right
                    ]
            , rightMetadata
            )

        NumberNode (NumberAddition _ (Just nodeA) Nothing) ->
            let
                ( left, leftMetadata ) =
                    viewNode (depth + 1) updatedMetadata (NumberNode nodeA)
            in
            ( renderedNode :: left
            , leftMetadata
            )

        NumberNode (NumberAddition _ Nothing (Just nodeB)) ->
            let
                ( right, righMetadata ) =
                    viewNode (depth + 1) updatedMetadata (NumberNode nodeB)
            in
            ( renderedNode :: right
            , righMetadata
            )

        NumberNode (NumberAddition _ Nothing Nothing) ->
            ( [ renderedNode ]
            , updatedMetadata
            )


buildMetadata : ( Float, Float ) -> Graph -> Metadata
buildMetadata ( width, height ) graph =
    let
        buildNodeMetadata : Node -> Int -> Metadata -> Metadata
        buildNodeMetadata node depth metadata =
            let
                updatedMetadata =
                    { metadata
                        | itemCount =
                            if Array.length metadata.itemCount <= depth then
                                Array.push { total = 1, placed = 0, height = Node.height node, occupied = 0 } metadata.itemCount

                            else
                                Array.set depth
                                    (Array.get depth metadata.itemCount
                                        |> Maybe.map (\count -> { count | total = count.total + 1, height = count.height + Node.height node + verticalGap })
                                        |> Maybe.withDefault { total = 1, placed = 0, height = Node.height node, occupied = 0 }
                                    )
                                    metadata.itemCount
                    }
            in
            case node of
                NumberNode (NumberConstant _ _) ->
                    updatedMetadata

                NumberNode (NumberAddition _ (Just nodeA) (Just nodeB)) ->
                    updatedMetadata
                        |> buildNodeMetadata (NumberNode nodeA) (depth + 1)
                        |> buildNodeMetadata (NumberNode nodeB) (depth + 1)

                NumberNode (NumberAddition _ (Just nodeA) Nothing) ->
                    buildNodeMetadata (NumberNode nodeA) (depth + 1) updatedMetadata

                NumberNode (NumberAddition _ Nothing (Just nodeB)) ->
                    buildNodeMetadata (NumberNode nodeB) (depth + 1) updatedMetadata

                NumberNode (NumberAddition _ Nothing Nothing) ->
                    updatedMetadata
    in
    buildNodeMetadata graph 0 { itemCount = Array.empty, size = { width = width, height = height } }
