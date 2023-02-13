module Interpreter exposing (..)

import Expression exposing (Expression(..), NumberExpression(..))


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

        NumberAddition left right ->
            interpretNumber left + interpretNumber right

        NumberMultiplication left right ->
            interpretNumber left * interpretNumber right
