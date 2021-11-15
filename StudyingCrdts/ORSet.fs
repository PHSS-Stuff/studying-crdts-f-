module StudyingCrdts.ORSet

type ORSet<'a when 'a: comparison> = Map<'a, VClock.VTime> *  Map<'a, VClock.VTime>
let zero: ORSet<'a> = (Map.empty, Map.empty)

let value (add,rem) =
    rem |> Map.fold (fun acc k vr ->
        match Map.tryFind k acc with
        | Some va when VClock.compare va vr = VClock.Ord.Lt -> Map.remove k acc
        | _ -> acc ) add
    
let add r e (add,rem) =
    match Map.tryFind e add, Map.tryFind e rem with
    | Some v, _ -> Map.add e (VClock.inc r v) add ,  Map.remove e rem
    | _, Some v -> Map.add e (VClock.inc r v) add ,  Map.remove e rem
    | _, _ -> Map.add e (VClock.inc r VClock.zero) add ,  Map.remove e rem

let rem r e (add,rem) =
    match Map.tryFind e add, Map.tryFind e rem with
    | Some v, _ -> Map.remove e add , Map.add e (VClock.inc r v) rem
    | _, Some v -> Map.remove e add, Map.add e (VClock.inc r v) rem
    | _, _ -> Map.remove e add, Map.add e (VClock.inc r VClock.zero) rem
    
let merge (add1,rem1) (add2, rem2) =
    let mergeKeys a b =
        b |> Map.fold(fun acc k vb ->
            match Map.tryFind k acc with
            | Some v -> Map.add k (VClock.merge v vb) acc
            | _ -> acc) a
    let addKeys = mergeKeys add1 add2
    let remKeys = mergeKeys rem1 rem2
    let add = remKeys |> Map.fold(fun acc k vr ->
        match Map.tryFind k acc with
        | Some v when VClock.compare v vr = VClock.Ord.Lt -> Map.remove k acc
        | _ -> acc ) addKeys
    let rem = addKeys |> Map.fold(fun acc k va ->
        match Map.tryFind k acc with
        | Some v when VClock.compare va v <> VClock.Ord.Lt -> Map.remove k acc
        | _ -> acc) remKeys
    (add,rem)