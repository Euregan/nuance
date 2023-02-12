module Graph.Links exposing (Link, Links, Position, empty, get, insert, mapToList, singleton, union)

import Dict exposing (Dict)
import UUID exposing (UUID)


type Links
    = Links (Dict String Link)


type alias Position =
    { x : Float
    , y : Float
    }


type alias Link =
    { output : Position
    , input : Position
    }


toString : ( UUID, UUID ) -> String
toString ( output, input ) =
    UUID.toString output ++ UUID.toString input


empty : Links
empty =
    Links Dict.empty


singleton : ( UUID, UUID ) -> Link -> Links
singleton ids error =
    Links <| Dict.singleton (toString ids) error


insert : ( UUID, UUID ) -> Link -> Links -> Links
insert ids error (Links errors) =
    Links <| Dict.insert (toString ids) error errors


union : Links -> Links -> Links
union (Links left) (Links right) =
    Links <| Dict.union left right


get : ( UUID, UUID ) -> Links -> Maybe Link
get ids (Links links) =
    Dict.get (toString ids) links


mapToList : (Link -> b) -> Links -> List b
mapToList fun (Links links) =
    Dict.values links |> List.map fun
