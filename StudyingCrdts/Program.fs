// Learn more about F# at http://fsharp.org

open System

[<EntryPoint>]
let main argv =
    let mySequence = seq{ for i in 1..10 do yield i * i }
    printfn "%A" mySequence
    StudyingCrdts.PNCounter.function2<Int64> 5L 2L 
    printfn "Hello World from F#!"
    0 // return an integer exit code
