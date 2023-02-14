module Interpreter exposing (..)

import Expression exposing (Expression(..), NumberExpression(..))
import Node exposing (Node)


type Result
    = NumberResult Float


interpret : Expression -> Result
interpret expression =
    case expression of
        NumberExpression numberExpression ->
            NumberResult <| interpretNumber numberExpression


interpretNumber : NumberExpression -> Float
interpretNumber expression =
    case expression of
        NumberConstant number ->
            number

        NumberBinary Node.NumberAddition left right ->
            interpretNumber left + interpretNumber right

        NumberBinary Node.NumberMultiplication left right ->
            interpretNumber left * interpretNumber right
