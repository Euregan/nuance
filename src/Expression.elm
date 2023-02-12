module Expression exposing (fromNode)

import Expression.Errors as Errors exposing (Errors)
import Node exposing (Node(..), NumberNode(..))


type Expression
    = NumberExpression NumberExpression


type NumberExpression
    = NumberConstant Float
    | NumberAddition NumberExpression NumberExpression


fromNode : Node -> Result Errors Expression
fromNode node =
    case node of
        NumberNode numberNode ->
            Result.map NumberExpression <| fromNumberNode numberNode


fromNumberNode : NumberNode -> Result Errors NumberExpression
fromNumberNode node =
    case node of
        Node.NumberConstant _ (Just constant) ->
            Ok (NumberConstant constant)

        Node.NumberConstant id Nothing ->
            Err <| Errors.singleton id "This constant needs to have a value"

        Node.NumberAddition _ (Just left) (Just right) ->
            case ( fromNumberNode left, fromNumberNode right ) of
                ( Ok leftExpression, Ok rightExpression ) ->
                    Ok <| NumberAddition leftExpression rightExpression

                ( Err leftErrors, Err rightErrors ) ->
                    Err <| Errors.union leftErrors rightErrors

                ( Err leftErrors, Ok _ ) ->
                    Err leftErrors

                ( Ok _, Err rightErrors ) ->
                    Err rightErrors

        Node.NumberAddition id (Just left) Nothing ->
            Err <|
                Errors.union
                    (Errors.singleton id "This addition needs to have two values")
                    (case fromNumberNode left of
                        Ok _ ->
                            Errors.empty

                        Err errors ->
                            errors
                    )

        Node.NumberAddition id Nothing (Just right) ->
            Err <|
                Errors.union
                    (Errors.singleton id "This addition needs to have two values")
                    (case fromNumberNode right of
                        Ok _ ->
                            Errors.empty

                        Err errors ->
                            errors
                    )

        Node.NumberAddition id Nothing Nothing ->
            Err <| Errors.singleton id "This addition needs to have two values"
