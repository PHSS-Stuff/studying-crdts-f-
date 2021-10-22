module GCounter
type ReplicaId = string
type GCounter = Map<ReplicaId,int64>
let zero: GCounter = Map.empty
let value(c: GCounter) =
    c |> Map.fold (fun acc _ v -> acc + v) 0L
let inc r (c:GCounter) =
    match Map.tryFind r c with
        | Some x -> Map.add r (x + 1L) c
        | None -> Map.add r 1L c
let merge (a:GCounter) (b:GCounter) =
    a |> Map.fold (fun acc ka va ->
        match Map.tryFind ka acc with
        | Some vb -> Map.add ka (max va vb) acc
        | None -> Map.add ka  va acc) b