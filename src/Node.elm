module Node exposing (..)

import Html exposing (Html, div, input, option, select, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onInput)
import UUID exposing (UUID)


type alias Actions msg =
    { run : msg
    , add : UUID -> (UUID -> Node) -> msg
    , replace : UUID -> Node -> msg
    }


type State a
    = Error String
    | Result a
    | ErrorFurtherDown


type alias Metadata a =
    { id : UUID
    , state : State a
    }


type Node
    = NumberNode NumberNode


type NumberNode
    = NumberGhost (Metadata Float)
    | NumberConstant (Metadata Float) (Maybe Float)
    | NumberAddition (Metadata Float) (Maybe NumberNode) (Maybe NumberNode)


error : State a -> Maybe String
error state =
    case state of
        Error err ->
            Just err

        Result _ ->
            Nothing

        ErrorFurtherDown ->
            Nothing


result : (a -> String) -> State a -> Maybe String
result toString state =
    case state of
        Error _ ->
            Nothing

        Result value ->
            Just <| toString value

        ErrorFurtherDown ->
            Nothing


lines : Int -> Float
lines count =
    toFloat count * 40 + (toFloat count - 1) * 10


height : Node -> Float
height node =
    case node of
        NumberNode (NumberGhost _) ->
            lines 2

        NumberNode (NumberConstant _ _) ->
            lines 1

        NumberNode (NumberAddition _ _ _) ->
            lines 2


view : Node -> Actions msg -> Html msg
view node actions =
    case node of
        NumberNode numberNode ->
            numberView numberNode actions


numberView : NumberNode -> Actions msg -> Html msg
numberView node actions =
    div [ class "w-full h-full bg-red-600 p-2" ]
        [ case node of
            NumberGhost metadata ->
                div []
                    [ text "Pick one"
                    , select
                        [ onInput
                            (\value ->
                                case value of
                                    "constant" ->
                                        actions.replace metadata.id (NumberNode (NumberConstant { id = metadata.id, state = Error "Missing a value" } Nothing))

                                    "addition" ->
                                        actions.replace metadata.id (NumberNode (NumberAddition { id = metadata.id, state = Error "Missing values" } Nothing Nothing))

                                    _ ->
                                        actions.replace metadata.id (NumberNode node)
                            )
                        ]
                        [ option [ value "constant" ] [ text "Constant" ]
                        , option [ value "addition" ] [ text "Addition" ]
                        ]
                    ]

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
        NumberGhost _ ->
            1

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
