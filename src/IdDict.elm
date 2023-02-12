module IdDict exposing (IdDict, empty, get, insert, mapToList, singleton, union)

import Dict exposing (Dict)
import UUID exposing (UUID)


type IdDict a
    = IdDict (Dict String a)


empty : IdDict a
empty =
    IdDict Dict.empty


singleton : UUID -> a -> IdDict a
singleton id value =
    IdDict <| Dict.singleton (UUID.toString id) value


insert : UUID -> a -> IdDict a -> IdDict a
insert id value (IdDict dict) =
    IdDict <| Dict.insert (UUID.toString id) value dict


union : IdDict a -> IdDict a -> IdDict a
union (IdDict left) (IdDict right) =
    IdDict <| Dict.union left right


get : UUID -> IdDict a -> Maybe a
get id (IdDict dict) =
    Dict.get (UUID.toString id) dict


mapToList : (a -> b) -> IdDict a -> List b
mapToList fun (IdDict dict) =
    Dict.values dict |> List.map fun
