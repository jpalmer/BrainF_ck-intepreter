let main() =
    let w = System.Diagnostics.Stopwatch.StartNew()
    let proc = System.Diagnostics.Process.Start("interp.exe");
    let mutable count = 0
    let buf = Array.create 10000 'a'
    while count < 100000 do
        count <- count + proc.StandardOutput.ReadBlock(buf,0,9999)
    proc.Kill()
    w.Stop()
    printf "%i ticks = %i kticks" w.ElapsedTicks (w.ElapsedTicks / 1000L)