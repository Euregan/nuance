module Node exposing (..)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, type_, value)
import UUID exposing (UUID)


type State a
    = Error String
    | Result a


type alias Metadata a =
    { id : UUID
    , state : State a
    }


type Node
    = NumberNode NumberNode


type NumberNode
    = NumberConstant (Metadata Float) (Maybe Float)
    | NumberAddition (Metadata Float) (Maybe NumberNode) (Maybe NumberNode)


error : State a -> Maybe String
error state =
    case state of
        Error err ->
            Just err

        Result _ ->
            Nothing


result : (a -> String) -> State a -> Maybe String
result toString state =
    case state of
        Error _ ->
            Nothing

        Result value ->
            Just <| toString value


lines : Int -> Float
lines count =
    toFloat count * 40 + (toFloat count - 1) * 10


height : Node -> Float
height node =
    case node of
        NumberNode (NumberConstant _ _) ->
            lines 1

        NumberNode (NumberAddition _ _ _) ->
            lines 2


view : Node -> Html msg
view node =
    case node of
        NumberNode numberNode ->
            numberView numberNode


numberView : NumberNode -> Html msg
numberView node =
    div [ class "w-full h-full bg-red-600 p-2" ]
        [ case node of
            NumberConstant _ constant ->
                input [ type_ "number", value <| Maybe.withDefault "" <| Maybe.map String.fromFloat constant ] []

            NumberAddition _ _ _ ->
                text "Addition"
        ]


depth : Node -> Int
depth node =
    case node of
        NumberNode numberNode ->
            numberDepth numberNode


numberDepth : NumberNode -> Int
numberDepth node =
    case node of
        NumberConstant _ _ ->
            1

        NumberAddition _ (Just inputA) (Just inputB) ->
            1 + max (numberDepth inputA) (numberDepth inputB)

        NumberAddition _ (Just inputA) Nothing ->
            1 + numberDepth inputA

        NumberAddition _ Nothing (Just inputB) ->
            1 + numberDepth inputB

        NumberAddition _ Nothing Nothing ->
            1
