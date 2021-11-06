module StudyingCrdts.GCounter

type ReplicaId = string
type GCounter = Map<ReplicaId,int64>
let zero:GCounter = Map.empty
let value (c:GCounter) =
    c |> Map.fold (fun acc _ v -> acc + v ) 0L
let inc r (c:GCounter) : GCounter =
    match Map.tryFind r c with
    | Some v -> Map.add r (v + 1L) c
    | None -> Map.add r 1L c
let merge (a:GCounter) (b:GCounter):GCounter =
    b |> Map.fold (fun acc kb vb ->
        match Map.tryFind kb acc with
        | Some va -> Map.add kb (max va vb) acc
        | None -> Map.add kb vb acc) a