
let mutable times = []
let main() =
    let w = System.Diagnostics.Stopwatch.StartNew()
    let proc = new System.Diagnostics.Process()
    proc.StartInfo.UseShellExecute <- false
    proc.StartInfo.RedirectStandardOutput <- true
    proc.StartInfo.FileName <- "interp.exe"
    proc.Start() |> ignore
    let mutable count = 0
    let buf = Array.create 10000 'a'
    while count < 100000 do
        count <- count + proc.StandardOutput.ReadBlock(buf,0,9999)
    proc.Kill()
    w.Stop()
    let time = w.ElapsedTicks
    times <- (time|>float)::times
    printfn "%i ticks = %i kticks = %fMticks" time (time /1000L) ((time |> float) / (1000.0*1000.0)) 
    
[0..10] |> List.iter (fun t -> main())

let average = times |> List.average
printfn "average"
printfn "%f ticks = %f kticks = %fMticks" average (average /1000.0) (average / (1000.0*1000.0)) 
