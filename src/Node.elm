module Node exposing (..)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, type_, value)


type Node
    = NumberNode NumberNode


type NumberNode
    = NumberConstant (Maybe Float)
    | NumberAddition (Maybe NumberNode) (Maybe NumberNode)


view : Node -> Html msg
view node =
    case node of
        NumberNode numberNode ->
            numberView numberNode


numberView : NumberNode -> Html msg
numberView node =
    case node of
        NumberConstant constant ->
            div [ class "w-24 h-24" ] [ input [ type_ "number", value <| Maybe.withDefault "" <| Maybe.map String.fromFloat constant ] [] ]

        NumberAddition _ _ ->
            div [ class "w-24 h-24" ] [ text "Addition" ]


depth : Node -> Int
depth node =
    case node of
        NumberNode numberNode ->
            numberDepth numberNode


numberDepth : NumberNode -> Int
numberDepth node =
    case node of
        NumberConstant _ ->
            1

        NumberAddition (Just inputA) (Just inputB) ->
            1 + max (numberDepth inputA) (numberDepth inputB)

        NumberAddition (Just inputA) Nothing ->
            1 + numberDepth inputA

        NumberAddition Nothing (Just inputB) ->
            1 + numberDepth inputB

        NumberAddition Nothing Nothing ->
            1
