module StudyingCrdts.VClock

type Ord =
    | Lt = -1
    | Eq = 0
    | Gt = 1
    | Cc = 2
    
type VTime = GCounter.GCounter
let zero : VTime = GCounter.zero
let value = GCounter.value
let inc = GCounter.inc
let merge = GCounter.merge
let compare (a: VTime) (b: VTime) : Ord =
    let valOrDefault k map =
        match Map.tryFind k map with
        | Some v -> v
        | None -> 0L
    let aKeys = a|> Map.toSeq |> Seq.map fst |> Set.ofSeq
    let bKeys = b |> Map.toSeq |> Seq.map fst |> Set.ofSeq
    (aKeys + bKeys)
    |> Seq.fold (fun prev k ->
            let va = valOrDefault k a
            let vb = valOrDefault k b
            match prev with
            | Ord.Eq when va > vb -> Ord.Gt
            | Ord.Eq when va < vb -> Ord.Lt
            | Ord.Lt when va > vb -> Ord.Cc
            | Ord.Gt when va < vb -> Ord.Cc
            | _ -> prev
        ) Ord.Eq