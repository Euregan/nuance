module Graph exposing (..)

import Html exposing (Html)
import Node exposing (Node(..), NumberNode(..))
import Svg exposing (foreignObject, svg)
import Svg.Attributes exposing (class, x, y)


type alias Graph =
    Node


view : Graph -> Html msg
view graph =
    svg [] <| viewNode 0 graph


viewNode : Int -> Node -> List (Html msg)
viewNode depth node =
    foreignObject
        [ class <| "h-1 w-1 overflow-visible"
        , x <| String.fromInt <| depth * 240
        , y <| String.fromFloat 40
        ]
        [ Node.view node ]
        :: (case node of
                NumberNode numberNode ->
                    viewNumberNode depth numberNode
           )


viewNumberNode : Int -> NumberNode -> List (Html msg)
viewNumberNode depth node =
    case node of
        NumberConstant _ ->
            []

        NumberAddition (Just nodeA) (Just nodeB) ->
            List.concat
                [ viewNode (depth + 1) (NumberNode nodeA)
                , viewNode (depth + 1) (NumberNode nodeB)
                ]

        NumberAddition (Just nodeA) Nothing ->
            List.concat
                [ viewNode (depth + 1) (NumberNode nodeA)
                ]

        NumberAddition Nothing (Just nodeB) ->
            List.concat
                [ viewNode (depth + 1) (NumberNode nodeB)
                ]

        NumberAddition Nothing Nothing ->
            []
