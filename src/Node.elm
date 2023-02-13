module Node exposing (..)

import Html exposing (Html, div, input, option, select, text)
import Html.Attributes exposing (class, disabled, selected, type_, value)
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
    | Pending


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

        Pending ->
            Nothing


resultAsString : (a -> String) -> State a -> Maybe String
resultAsString toString state =
    Maybe.map toString <| result state


result : State a -> Maybe a
result state =
    case state of
        Error _ ->
            Nothing

        Result value ->
            Just value

        ErrorFurtherDown ->
            Nothing

        Pending ->
            Nothing


numberNodeResult : NumberNode -> Maybe Float
numberNodeResult node =
    case node of
        NumberGhost _ ->
            Nothing

        NumberConstant { state } _ ->
            result state

        NumberAddition { state } _ _ ->
            result state


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
                                        actions.replace metadata.id <| validate <| NumberNode (NumberConstant { id = metadata.id, state = Pending } Nothing)

                                    "addition" ->
                                        actions.replace metadata.id <| validate <| NumberNode (NumberAddition { id = metadata.id, state = Pending } Nothing Nothing)

                                    _ ->
                                        actions.replace metadata.id <| validate <| NumberNode node
                            )
                        ]
                        [ option [ disabled True, selected True ] [ text "Pick a node" ]
                        , option [ value "constant" ] [ text "Constant" ]
                        , option [ value "addition" ] [ text "Addition" ]
                        ]
                    ]

            NumberConstant metadata constant ->
                input
                    [ type_ "number"
                    , value <| Maybe.withDefault "" <| Maybe.map String.fromFloat constant
                    , onInput (\value -> actions.replace metadata.id <| (NumberNode <| NumberConstant metadata <| String.toFloat value))
                    ]
                    []

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

        NumberAddition _ (Just left) (Just right) ->
            1 + max (numberDepth left) (numberDepth right)

        NumberAddition _ (Just left) Nothing ->
            1 + numberDepth left

        NumberAddition _ Nothing (Just right) ->
            1 + numberDepth right

        NumberAddition _ Nothing Nothing ->
            1


validate : Node -> Node
validate node =
    case node of
        NumberNode numberNode ->
            NumberNode <| validateNumber numberNode


validateNumber : NumberNode -> NumberNode
validateNumber node =
    case node of
        NumberGhost _ ->
            node

        NumberConstant metadata (Just constant) ->
            NumberConstant { metadata | state = Result constant } (Just constant)

        NumberConstant metadata Nothing ->
            NumberConstant { metadata | state = Error "Missing the value" } Nothing

        NumberAddition metadata (Just left) (Just right) ->
            NumberAddition
                { metadata
                    | state =
                        case Maybe.map2 (\leftResult rightResult -> leftResult + rightResult) (numberNodeResult left) (numberNodeResult right) of
                            Just number ->
                                Result number

                            Nothing ->
                                ErrorFurtherDown
                }
                (Just left)
                (Just right)

        NumberAddition metadata (Just left) Nothing ->
            NumberAddition { metadata | state = Error "Missing a value" } (Just left) Nothing

        NumberAddition metadata Nothing (Just right) ->
            NumberAddition { metadata | state = Error "Missing a value" } Nothing (Just right)

        NumberAddition metadata Nothing Nothing ->
            NumberAddition { metadata | state = Error "Missing both values" } Nothing Nothing
