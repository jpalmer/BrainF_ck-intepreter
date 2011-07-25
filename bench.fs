
let mutable times = []
let main file arg ()=
    let w = System.Diagnostics.Stopwatch.StartNew()
    let proc = new System.Diagnostics.Process()
    proc.StartInfo.UseShellExecute <- false
    proc.StartInfo.RedirectStandardOutput <- true
    proc.StartInfo.Arguments <- arg
    proc.StartInfo.FileName <- file
    proc.Start() |> ignore
    let mutable count = 0
    let buf = Array.create 10000 'a'
    while count < 100000 do
        count <- count + proc.StandardOutput.ReadBlock(buf,0,9999)
    proc.Kill()
    w.Stop()
    let time = w.ElapsedTicks
    times <- (time|>float)::times

let bmark file arg comment () =
    [0..2] |> List.iter (fun t -> main file arg ())
    
    let average = times |> List.average
    printfn "average for %s" comment
    printfn "%f ticks = %f kticks = %fMticks" average (average /1000.0) (average / (1000.0*1000.0)) 

bmark "interp.exe" "" "asm interpreter" ()
bmark "mono" "--optimize=all fsharp_interp.exe" "Fsharp interpreter" ()
