module Expression exposing (Expression(..), NumberExpression(..), fromNode)

import Expression.Errors as Errors exposing (Errors)
import Node exposing (Node(..), NumberNode(..))


type Expression
    = NumberExpression NumberExpression


type NumberExpression
    = NumberConstant Float
    | NumberBinary Node.NumberBinary NumberExpression NumberExpression


fromNode : Node -> Result Errors Expression
fromNode node =
    case node of
        NumberNode numberNode ->
            Result.map NumberExpression <| fromNumberNode numberNode


fromNumberNode : NumberNode -> Result Errors NumberExpression
fromNumberNode node =
    case node of
        Node.NumberGhost { id } ->
            Err <| Errors.singleton id "This node hasn't been set up"

        Node.NumberConstant _ (Just constant) ->
            Ok (NumberConstant constant)

        Node.NumberConstant { id } Nothing ->
            Err <| Errors.singleton id "This constant needs to have a value"

        Node.NumberBinary _ kind (Just left) (Just right) ->
            case ( fromNumberNode left, fromNumberNode right ) of
                ( Ok leftExpression, Ok rightExpression ) ->
                    Ok <| NumberBinary kind leftExpression rightExpression

                ( Err leftErrors, Err rightErrors ) ->
                    Err <| Errors.union leftErrors rightErrors

                ( Err leftErrors, Ok _ ) ->
                    Err leftErrors

                ( Ok _, Err rightErrors ) ->
                    Err rightErrors

        Node.NumberBinary { id } _ (Just left) Nothing ->
            Err <|
                Errors.union
                    (Errors.singleton id "This needs to have two values")
                    (case fromNumberNode left of
                        Ok _ ->
                            Errors.empty

                        Err errors ->
                            errors
                    )

        Node.NumberBinary { id } _ Nothing (Just right) ->
            Err <|
                Errors.union
                    (Errors.singleton id "This needs to have two values")
                    (case fromNumberNode right of
                        Ok _ ->
                            Errors.empty

                        Err errors ->
                            errors
                    )

        Node.NumberBinary { id } _ Nothing Nothing ->
            Err <| Errors.singleton id "This needs to have two values"
