// Learn more about F# at http://fsharp.net

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
    static member fromstring x=
        match x with
        |'>' -> Incdata dval
        |'<' -> Decdata dval
        |'+' -> Incbyte dval
        |'-' -> Decbyte dval
        |'.' -> Output
        |'[' -> Whilestart (0,0)
        |']' -> Whileend (0,0)
    static member print x =
        match x with
        |Incdata(t) -> sprintf "inc %i" t
        |Decdata(t) -> sprintf "dec %i" t
        |Incbyte(t) -> sprintf "incb %i" t
        |Decbyte(t) -> sprintf "decb %i" t
        |Output -> "out"
        |Whilestart(_) -> "start"
        |Whileend(_) -> "end"

let tokenize (string:string) = 
    let ret = string.ToCharArray() |> Array.map (chars.fromstring)
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
  //  ret |> Array.iter (fun t -> eprintfn "%s" (chars.print t))
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
        |Output -> outflag <- true; tmp <- (program.[index] :: tmp)
        |Whilestart(_) | Whileend(_) ->
            if tmp <> [] then
                if outflag then out <- Outputb(tmp) :: out
                else out <- Compute(tmp) :: out
            out <- Whiles(program.[index]) :: out
            tmp<- []
    if tmp <> [] then
        if outflag then out <- Outputb(tmp) :: out
        else out <- Compute(tmp) :: out
    out

let compilecompute block =
    let offset = ref 0
    let r = 
        block
        |>List.map
            (fun t -> 
                match t with
                |Incdata(t) -> offset := !offset + t;""
                |Decdata(t) -> offset := !offset - t;""
                |Incbyte(t) -> sprintf "add QWORD [rbx+%i],%i" (!offset*8) t
                |Decbyte(t) -> sprintf "sub QWORD [rbx+%i],%i" (!offset*8) t)
        |> List.filter (fun t -> t <> "") 
    if !offset <> 0 then r @ (sprintf "add rbx, %i" (!offset * 8) :: []) else r
//next optimisation idea
//create 'blocks'
//      these are delimited by [ and ]
//      so blocks are a sequence of <>+- .
// use slow optimisation if block contains . (output is not in a tight loop)
// otherwise it is possible to figure out every offset needed
let makeasm prog =
    prog
    |>List.map     
       (fun (elem) ->
        match elem with
        |Incdata(t) ->
            let offset = t*8
            sprintf "    add rbx, %i" offset
        |Decdata(t) ->
            let offset = t*8
            sprintf "    sub rbx, %i" offset
        |Incbyte(t) ->
                if t = 1 then "    inc QWORD [rbx]" else sprintf "   add QWORD [rbx], %i " t
        |Decbyte(t) ->
                if t = 1 then "    dec QWORD [rbx]" else sprintf "   sub QWORD [rbx], %i " t
        |Output ->"    push rbx 
    mov vd, [rbx]
        call putchar
    pop rbx"
        |Whilestart(start,ed) -> //since decbyte, incbyte and lrlm modify the flags registers, we don't need to do the test here
           sprintf"    mov rax, [rbx]
    and QWORD rax, rax
    jz labelend%i
    labelstart%i:" ed start

        |Whileend(start,ed) -> //whileend sequences can occur (once about 1/2 way through program),
                                //if you make it past the first whileend instruction you will always make it past all the rest, so no need for checks.
            sprintf"    mov rax,[rbx]
    and rax,rax
    jnz labelstart%i
    labelend%i:" start ed
                 )
let compileComplete = 
    List.collect
        (fun t -> 
            match t  with
            |Compute(t) -> compilecompute t
            |Outputb(t) -> makeasm t 
            |Whiles(t) -> makeasm (t::[]) )
let program = ">++++++++++>>>+>+[>>>+[-[<<<<<[+<<<<<]>>[[-]>[<<+>+>-]<[>+<-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>>>>+>+<<<<<<-[>+<-]]]]]]]]]]]>[<+>-]+>>>>>]<<<<<[<<<<<]>>>>>>>[>>>>>]++[-<<<<<]>>>>>>-]+>>>>>]<[>++<-]<<<<[<[>+<-]<<<<]>>[->[-]++++++[<++++++++>-]>>>>]<<<<<[<[>+>+<<-]>.<<<<<]>.>>>>]" //currently at 309 lines (for reference there are 294 brinf*ck instructions
let header = System.IO.File.ReadAllText("header.txt")
printfn "%s" header
program |> tokenize |> optimize |> fixwhiles |> Blockify |> compileComplete |> List.iter (fun t -> printfn "%s" t)
let footer = "xor rax,rax ;return 0
	leave
    ret"
printfn "%s" footer
