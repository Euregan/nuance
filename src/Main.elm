module Main exposing (..)

import Browser
import Expression exposing (Expression)
import Expression.Errors exposing (Errors)
import Graph exposing (Graph)
import Html exposing (Html)
import Interpreter exposing (interpret)
import Node exposing (Node(..), NumberNode(..), State(..))
import Random
import UUID


main : Program Flags Model Msg
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
    , lastCompilation : Result Errors Expression
    , lastResult : Maybe Interpreter.Result
    }


type alias Flags =
    { size : ( Float, Float )
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        graph =
            NumberNode
                (NumberAddition
                    { id = Random.step UUID.generator (Random.initialSeed 7) |> Tuple.first
                    , state = ErrorFurtherDown
                    }
                    (Just
                        (NumberAddition
                            { id = Random.step UUID.generator (Random.initialSeed 1) |> Tuple.first
                            , state = Result 8
                            }
                            (Just
                                (NumberConstant
                                    { id = Random.step UUID.generator (Random.initialSeed 2) |> Tuple.first
                                    , state = Result 1
                                    }
                                    (Just 1)
                                )
                            )
                            (Just
                                (NumberAddition
                                    { id = Random.step UUID.generator (Random.initialSeed 534) |> Tuple.first
                                    , state = Result 7
                                    }
                                    (Just
                                        (NumberConstant
                                            { id = Random.step UUID.generator (Random.initialSeed 89) |> Tuple.first
                                            , state = Result 3
                                            }
                                            (Just 3)
                                        )
                                    )
                                    (Just
                                        (NumberConstant
                                            { id = Random.step UUID.generator (Random.initialSeed 23) |> Tuple.first
                                            , state = Result 4
                                            }
                                            (Just 4)
                                        )
                                    )
                                )
                            )
                        )
                    )
                    (Just
                        (NumberAddition
                            { id = Random.step UUID.generator (Random.initialSeed 4) |> Tuple.first
                            , state = Error "Missing a value"
                            }
                            (Just
                                (NumberConstant
                                    { id = Random.step UUID.generator (Random.initialSeed 5) |> Tuple.first
                                    , state = Result 3
                                    }
                                    (Just 3)
                                )
                            )
                            Nothing
                        )
                    )
                )
    in
    ( { size = flags.size
      , lastCompilation = Expression.fromNode graph
      , lastResult = Nothing
      , graph = graph
      }
    , Cmd.none
    )


type Msg
    = Run


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            let
                expression =
                    Expression.fromNode model.graph
            in
            ( { model
                | lastCompilation = expression
                , lastResult = expression |> Result.map interpret |> Result.toMaybe
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Graph.view model.size
        model.graph
        { run = Run
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
