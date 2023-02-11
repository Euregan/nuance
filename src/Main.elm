module Main exposing (..)

import Browser
import Graph exposing (Graph)
import Html exposing (Html)
import Node exposing (Node(..), NumberNode(..))


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { size : ( Float, Float )
    , graph : Graph
    }


type alias Flags =
    { size : ( Float, Float )
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { size = flags.size
      , graph =
            NumberNode
                (NumberAddition
                    (Just
                        (NumberAddition
                            (Just (NumberConstant (Just 1)))
                            (Just (NumberConstant (Just 2)))
                        )
                    )
                    (Just
                        (NumberAddition
                            (Just (NumberConstant (Just 3)))
                            (Just (NumberConstant (Just 4)))
                        )
                    )
                )
      }
    , Cmd.none
    )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Graph.view model.size model.graph


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
