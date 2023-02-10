module Main exposing (..)

import Browser
import Graph exposing (Graph)
import Html exposing (Html)
import Node exposing (Node(..), NumberNode(..))


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    Graph


init : Model
init =
    NumberNode (NumberAddition (Just (NumberConstant (Just 42))) (Just (NumberConstant (Just 42))))


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    Graph.view model
