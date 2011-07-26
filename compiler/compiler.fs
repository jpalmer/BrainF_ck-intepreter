// Learn more about F# at http://fsharp.net

let opt = true
let dval = if opt then 0 else 1
type chars = 
    |Incdata of int
    |Decdata of int
    |Incbyte of int
    |Decbyte of int
    |Output
    |Input
    |Whilestart of int * int
    |Whileend of int * int
    |Lrlm //>+<- (should probably rename)
    |Blank
    static member fromstring x=
        match x with
        |'>' -> Incdata dval
        |'<' -> Decdata dval
        |'+' -> Incbyte dval
        |'-' -> Decbyte dval
        |'.' -> Output
        |',' -> Input
        |'[' -> Whilestart (0,0)
        |']' -> Whileend (0,0)
        |'?' -> Lrlm //correction done in tokeniser
        | _ -> Blank
    static member print x =
        match x with
        |Incdata(t) -> sprintf "inc %i" t
        |Decdata(t) -> sprintf "dec %i" t
        |Incbyte(t) -> sprintf "incb %i" t
        |Decbyte(t) -> sprintf "decb %i" t
        |Output -> "out"
        |Input -> ""
        |Whilestart(_) -> "start"
        |Whileend(_) -> "end"
        |Lrlm -> "lrlm"

let tokenize (string:string) = 
    let ret = string.Replace(">+<-","?").ToCharArray() |> Array.map (chars.fromstring) |> Array.filter(fun t -> t <> Blank)

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

let makeasm =
    Array.toList
    >> (fun t -> t.Head :: t) //without this one instruction gets lost due to pairwising.  
    >> List.toSeq //The way this processing is done could be made faster / more efficient, however the compiler is quite fast so no need yet
    >> Seq.pairwise
    >> Seq.map
     (fun (prev,elem) ->
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
            match prev with
            |Decbyte(_) |Incbyte(_) |Lrlm ->
                sprintf " jz labelend%i
    labelstart%i:" ed start
            |_ ->
            sprintf"    mov rax, [rbx]
    and QWORD rax, rax
    jz labelend%i
    labelstart%i:" ed start

        |Whileend(start,ed) -> //whileend sequences can occur (once about 1/2 way through program),
                                //if you make it past the first whileend instruction you will always make it past all the rest, so no need for checks.
            match prev with
            |Whileend(_) ->sprintf "    labelend%i:" ed
            |Decbyte(_) |Incbyte(_) |Lrlm ->
                sprintf"    jnz labelstart%i
    labelend%i:" start ed

            |_ ->
                sprintf"    mov rax,[rbx]
    and rax,rax
    jnz labelstart%i
    labelend%i:" start ed
        |Lrlm -> "    inc QWORD [rbx+8]
    dec QWORD [rbx]"
                 ) >> Seq.toArray
let program = ">++++++++++>>>+>+[>>>+[-[<<<<<[+<<<<<]>>[[-]>[<<+>+>-]<[>+<-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>>>>+>+<<<<<<-[>+<-]]]]]]]]]]]>[<+>-]+>>>>>]<<<<<[<<<<<]>>>>>>>[>>>>>]++[-<<<<<]>>>>>>-]+>>>>>]<[>++<-]<<<<[<[>+<-]<<<<]>>[->[-]++++++[<++++++++>-]>>>>]<<<<<[<[>+>+<<-]>.<<<<<]>.>>>>]" //currently at 309 lines (for reference there are 294 brinf*ck instructions
let header = System.IO.File.ReadAllText("header.txt")
printfn "%s" header
program |> tokenize |> optimize |> fixwhiles |> makeasm |> Array.iter (fun t -> printfn "%s" t)
let footer = "xor rax,rax ;return 0
	leave
    ret"
printfn "%s" footer
