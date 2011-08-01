module Main
open Initial
let outstr ="    push rbx 
    mov vd, [rbx]
        call putchar
    pop rbx"
    //unfortunately the compiler barfs on externally defined printf format strings
let ws: Printf.StringFormat<_>= "    mov rax, [rbx]
    and rax, rax
    jz labelend%i
    labelstart%i:"
let we: Printf.StringFormat<_>= "    mov rax,[rbx]
    and rax,rax
    jnz labelstart%i
    labelend%i:"
[<Literal>]
let memsize=8 //size of a memory cell in bytes - use 64 bit mem cells
type BlockEnd =
    |FlagsSet
    |FlagsUnset
    |Wend
let compilecompute block =
    let offset = ref 0
    let cleanfinish = ref FlagsUnset
    let r = 
        block
        |>List.map
            (fun t ->
                let offstr = 
                    if !offset > 0 then sprintf"+%i" (!offset*memsize)
                    else if !offset = 0 then ""
                    else sprintf "%i" (!offset*memsize)
                match t with
                |Incdata(t) -> offset := !offset + t; cleanfinish := FlagsUnset; ""
                |Decdata(t) -> offset := !offset - t; cleanfinish := FlagsUnset; ""
                |Incbyte(t) -> 
                    cleanfinish := FlagsSet; 
                    if t = 1 then sprintf "inc QWORD [rbx%s]" offstr
                    else sprintf "add QWORD [rbx%s],%i" offstr t
                |Decbyte(t) -> 
                    cleanfinish := FlagsSet; 
                    if t = 1 then sprintf "dec QWORD [rbx%s]" offstr
                    else sprintf "sub QWORD [rbx%s],%i" offstr t
                |Zero -> 
                    cleanfinish := FlagsUnset
                    "    mov QWORD [rbx], 0"
                |Rplm ->
                    cleanfinish := FlagsUnset
                    "mov rax, [rbx]
    add QWORD [rbx+8], rax
    mov QWORD [rbx],0"
                )
        |> List.filter (fun t -> t <> "") 
    if !offset > 0 then 
        r @ (sprintf "add rbx, %i" (!offset * memsize) :: []) ,FlagsUnset
    else if !offset < 0 then
        r @ (sprintf "sub rbx, %i" (abs(!offset * memsize)) :: []) ,FlagsUnset

    else (r, !cleanfinish)
   
//next optimisation idea
//create 'blocks'
//      these are delimited by [ and ]
//      so blocks are a sequence of <>+- .
// use slow optimisation if block contains . (output is not in a tight loop)
// otherwise it is possible to figure out every offset needed
let makeasm prog stat=
    prog
    |>List.map     
       (fun (elem) ->
        match elem with
        |Incdata(t) ->
            sprintf "    add rbx, %i" (t*memsize)
        |Decdata(t) ->
            sprintf "    sub rbx, %i" (t*memsize)
        |Incbyte(t) ->
                if t = 1 then "    inc QWORD [rbx]" else sprintf "   add QWORD [rbx], %i " t
        |Decbyte(t) ->
                if t = 1 then "    dec QWORD [rbx]" else sprintf "   sub QWORD [rbx], %i " t
        |Output -> outstr
        |Whilestart(start,ed) -> //since decbyte, incbyte and lrlm modify the flags registers, we don't need to do the test here
            match stat with
            |FlagsUnset | Wend ->
               sprintf ws ed start
            |FlagsSet ->
                sprintf "jz labelend%i
    labelstart%i:" ed start

        |Whileend(start,ed) -> //whileend sequences can occur (once about 1/2 way through program),
                                //if you make it past the first whileend instruction you will always make it past all the rest, so no need for checks.
            match stat with
            |FlagsUnset ->
                sprintf we start ed
            |FlagsSet -> 
                sprintf "   jnz labelstart%i
    labelend%i:" start ed
            |Wend -> sprintf "labelend%i:" ed     )
let compileComplete = 
    let prev = ref FlagsUnset
    List.collect
        (fun t -> 
            match t  with
            |Compute(t) -> 
                let p,s = compilecompute t; 
                prev := s; 
                p
            |Outputb(t) -> 
                prev := FlagsUnset;
                makeasm t (!prev)
            |Whiles(t) ->
                match t with
                |Whileend(_) ->
                    let r = makeasm (t::[]) (!prev) 
                    prev := Wend
                    r
                | _ -> 
                    let r = makeasm (t::[]) (!prev)
                    prev := FlagsSet
                    r)
//specially optimised sequences
//[-] mem[p] = 0 //(O(N)*O(dec) -> O(1))
//[>+<-] -> mem[p+1] += mem[p] ; mem[p] = 0 //(O(N)*O(dec) -> O(add))

//new ideas for things to optimise
//[ some combination of <,>,+,- ] where N(>) = N(<) - this is doable - there are 6 more loops that can be eliminated this way.  Will also get previous already done optimisations for free
//Ideas for how to do this
//      These sequences can always be modified to end with a '-'. (otherwise the loop will never end as BrainF_ck doesn't know about overflow
//      Once this is done run through the loop once in an interpreter.
//      Find which memory values were modified + by how much
//      Then simply convert loop to add / sub last
//should work
let program = ">++++++++++>>>+>+[>>>+[-[<<<<<[+<<<<<]>>[[-]>[<<+>+>-]<[>+<-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>>>>+>+<<<<<<-[>+<-]]]]]]]]]]]>[<+>-]+>>>>>]<<<<<[<<<<<]>>>>>>>[>>>>>]++[-<<<<<]>>>>>>-]+>>>>>]<[>++<-]<<<<[<[>+<-]<<<<]>>[->[-]++++++[<++++++++>-]>>>>]<<<<<[<[>+>+<<-]>.<<<<<]>.>>>>]" //currently at 283 lines (including header) (for reference there are 271 brinf*ck instructions
let header = System.IO.File.ReadAllText("header.txt")
printfn "%s" header
program |> tokenize |> optimize |> fixwhiles |> Blockify |> compileComplete |> List.iter (fun t -> printfn "%s" t)
let footer = "xor rax,rax ;return 0
	leave
    ret"
printfn "%s" footer
