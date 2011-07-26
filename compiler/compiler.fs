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
        | _ -> Blank


let tokenize (string:string) = 
    let ret = string.ToCharArray() |> Array.map (chars.fromstring) |> Array.filter(fun t -> t <> Blank)
    
    ret
let optimize (program:chars[])=
    //first optimize part - replace >>> with >(3)
    let mutable curpointer = -1
    let mutable pt = -1
    let mutable count = 0
    //inc data
    while curpointer+1 < program.Length do
        curpointer <- curpointer+1
        match program.[curpointer] with
        |Incdata(0) ->
            if pt = -1 then 
                pt <- curpointer
                count <- 1
            else count <- count + 1
        |_ -> 
            if pt <> -1 then
                program.[pt] <- Incdata(count)
                pt <- -1
            else ()
    curpointer <- -1
    //decdata
    while curpointer+1 < program.Length do
        curpointer <- curpointer+1
        match program.[curpointer] with
        |Decdata(0) ->
            if pt = -1 then 
                pt <- curpointer
                count <- 1
            else count <- count + 1
        |_ -> 
            if pt <> -1 then
                program.[pt] <- Decdata(count)
                pt <- -1
            else ()
    //incbyte
    curpointer <- -1
    while curpointer+1 < program.Length do
        curpointer <- curpointer+1
        match program.[curpointer] with
        |Incbyte(0) ->
            if pt = -1 then 
                pt <- curpointer
                count <- 1
            else count <- count + 1
        |_ -> 
            if pt <> -1 then
                program.[pt] <- Incbyte(count)
                pt <- -1
            else ()
    //decbyte
    curpointer <- -1
    while curpointer+1 < program.Length do
        curpointer <- curpointer+1
        match program.[curpointer] with
        |Decbyte(0) ->
            if pt = -1 then 
                pt <- curpointer
                count <- 1
            else count <- count + 1
        |_ -> 
            if pt <> -1 then
                program.[pt] <- Decbyte(count)
                pt <- -1
            else ()
    program |> Array.filter (fun t -> t <> Incdata(0) && t <> Decdata(0) && t <> Incbyte(0) && t <> Decbyte(0))

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
    >> (fun t -> t.Head :: t)
    >> List.toSeq
    >> Seq.pairwise
    >> Seq.map
     (fun (prev,elem) ->
        match elem with
        |Incdata(t) ->
            let offset = t*8
            sprintf "       add rbx, %i" offset
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
        |Whilestart(start,ed) ->
            match prev with
            |Decbyte(_) |Incbyte(_) ->
                sprintf " jz labelend%i
    labelstart%i:" ed start
            |_ ->
            sprintf"mov rax, [rbx]
    and QWORD rax, rax
    jz labelend%i
    labelstart%i:" ed start

        |Whileend(start,ed) -> //whileend is optimised based on the previous instruction
            match prev with
            |Whileend(_) ->sprintf "    labelend%i:" ed
            |Decbyte(_) |Incbyte(_) ->
                sprintf"    jnz labelstart%i
    labelend%i:" start ed

            |_ ->
                sprintf"    mov rax,[rbx]
    and rax,rax
    jnz labelstart%i
    labelend%i:" start ed
            
                 ) >> Seq.toArray
let program = ">++++++++++>>>+>+[>>>+[-[<<<<<[+<<<<<]>>[[-]>[<<+>+>-]<[>+<-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>>>>+>+<<<<<<-[>+<-]]]]]]]]]]]>[<+>-]+>>>>>]<<<<<[<<<<<]>>>>>>>[>>>>>]++[-<<<<<]>>>>>>-]+>>>>>]<[>++<-]<<<<[<[>+<-]<<<<]>>[->[-]++++++[<++++++++>-]>>>>]<<<<<[<[>+>+<<-]>.<<<<<]>.>>>>]" //currently at 361 lines
let header = System.IO.File.ReadAllText("header.txt")
printfn "%s" header
program |> tokenize |> optimize |> fixwhiles |> makeasm |> Array.iter (fun t -> printfn "%s" t)
let footer = "xor rax,rax ;return 0
	leave
    ret"
printfn "%s" footer
