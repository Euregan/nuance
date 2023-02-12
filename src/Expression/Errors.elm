module Expression.Errors exposing (Errors, empty, insert, singleton, union)

import Dict exposing (Dict)
import UUID exposing (UUID)


type Errors
    = Errors (Dict String String)


empty : Errors
empty =
    Errors Dict.empty


singleton : UUID -> String -> Errors
singleton id error =
    Errors <| Dict.singleton (UUID.toString id) error


insert : UUID -> String -> Errors -> Errors
insert id error (Errors errors) =
    Errors <| Dict.insert (UUID.toString id) error errors


union : Errors -> Errors -> Errors
union (Errors left) (Errors right) =
    Errors <| Dict.union left right
