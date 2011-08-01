module Initial
///Put some of the initial optimisations here to simplify things
let opt = true
let dval = if opt then 0 else 1
type chars = 
    |Incdata of int
    |Decdata of int
    |Incbyte of int
    |Decbyte of int
    |Output
    |Whilestart of int * int
    |Whileend of int * int
    |Zero
    |Rplm
    static member fromstring x=
        match x with
        |'>' -> Incdata dval
        |'<' -> Decdata dval
        |'+' -> Incbyte dval
        |'-' -> Decbyte dval
        |'.' -> Output
        |'[' -> Whilestart (0,0)
        |']' -> Whileend (0,0)
        |'0' -> Zero
        |'1' -> Rplm
    static member print x =
        match x with
        |Incdata(t) -> sprintf "inc %i" t
        |Decdata(t) -> sprintf "dec %i" t
        |Incbyte(t) -> sprintf "incb %i" t
        |Decbyte(t) -> sprintf "decb %i" t
        |Output -> "out"
        |Whilestart(_) -> "start"
        |Whileend(_) -> "end"
        |Zero -> "zero"

let tokenize (string:string) = 
    let ret = string.Replace("[-]","0").Replace("[>+<-]","1").ToCharArray() |> Array.map (chars.fromstring)
    ret
let optimize (program:chars[])=
    //really need to make this a bit cleverer - handle each instruction with a common function
    //first optimize part - replace >>> with >(3)
    let optimisepass opt outf= 
        let mutable curpointer = -1
        let mutable pt = -1
        let mutable count = 0
        while curpointer+1 < program.Length do
            curpointer <- curpointer+1
            
            if program.[curpointer] = opt then
                if pt = -1 then 
                    pt <- curpointer
                    count <- 1
                else count <- count + 1
            else 
                if pt <> -1 then
                    program.[pt] <- outf(count)
                    pt <- -1
                else ()
    optimisepass (Incdata(0)) (fun t -> Incdata(t))
    optimisepass (Decdata(0)) (fun t -> Decdata(t))
    optimisepass (Incbyte(0)) (fun t -> Incbyte(t))
    optimisepass (Decbyte(0)) (fun t -> Decbyte(t))
    let ret = program |> Array.filter (fun t -> t <> Incdata(0) && t <> Decdata(0) && t <> Incbyte(0) && t <> Decbyte(0))
    //uncomment below to print optimised instruction stream to stderr
    //ret |> Array.iter (fun t -> eprintfn "%s" (chars.print t))
    ret
let fixwhiles (program:chars[]) =
    let startstack = System.Collections.Generic.Stack<_>()
    let mutable curpointer = -1
    while curpointer+1 < program.Length do
        curpointer <- curpointer+1
        match program.[curpointer] with
        |Whilestart(_) -> startstack.Push(curpointer)
        |Whileend(_) ->
            let v = startstack.Pop()
            program.[v] <- Whilestart(v,curpointer)
            program.[curpointer] <- Whileend(v,curpointer)
        |_ -> () //do nothing for other stuff
    program
type Blocktypes =
    |Whiles of chars
    |Compute of chars list
    |Outputb of chars list
let Blockify (program:chars[]) =
    let mutable out = []
    let mutable tmp = []
    let mutable outflag = false
    let mutable index = -1
    while index < (program.Length-1) do
        index <- index + 1
        match program.[index] with
        |Incdata(_) |Decdata(_) |Incbyte(_) |Decbyte(_) -> tmp <- (program.[index] :: tmp) 
        |Zero | Rplm ->
            if tmp <> [] then
                if outflag then out <- Outputb(tmp |> List.rev) :: out
                else out <- Compute(tmp |> List.rev) :: out
            out <- Compute(program.[index]::[]) :: out
            tmp<- []

            
        |Output -> outflag <- true; tmp <- (program.[index] :: tmp)
        |Whilestart(_) | Whileend(_) ->
            if tmp <> [] then
                if outflag then out <- Outputb(tmp |> List.rev) :: out
                else out <- Compute(tmp |> List.rev) :: out
            out <- Whiles(program.[index]) :: out
            tmp<- []
    if tmp <> [] then
        if outflag then out <- Outputb(tmp) :: out
        else out <- Compute(tmp) :: out
    out |> List.rev
