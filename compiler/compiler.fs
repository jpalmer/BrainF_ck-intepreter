module Main
open Initial
//the strings below make the code that uses them a bit ugly
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
                    if t = 1 then sprintf "    inc QWORD [rbx%s]" offstr
                    else sprintf "    add QWORD [rbx%s],%i" offstr t
                |Decbyte(t) -> 
                    cleanfinish := FlagsSet; 
                    if t = 1 then sprintf "    dec QWORD [rbx%s]" offstr
                    else sprintf "    sub QWORD [rbx%s],%i" offstr t
                |Zero -> 
                    cleanfinish := FlagsUnset
                    "    mov QWORD [rbx], 0"
                |Rplm ->
                    cleanfinish := FlagsUnset
                    "    mov rax, [rbx]
    add QWORD [rbx+8], rax
    mov QWORD [rbx],0"
                |Lprm ->
                    cleanfinish := FlagsUnset
                    "    mov rax, [rbx]
    add QWORD [rbx-8], rax
    mov QWORD [rbx],0"
                |RpRpm ->
                    cleanfinish := FlagsUnset
                    "    mov rax, [rbx]
    add QWORD [rbx+8], rax
    add QWORD [rbx+16], rax
    mov QWORD [rbx],0"
                |LpLpm ->
                    cleanfinish := FlagsUnset
                    "    mov rax, [rbx]
    add QWORD [rbx-8], rax
    add QWORD [rbx-16], rax
    mov QWORD [rbx],0"
                |Output |Whilestart(_) |Whileend(_) -> failwith "invalid instruction in compute block"
                )
        |> List.filter (fun t -> t <> "") //incdata / decdata don't make instructions so filter them out 
    //now actually change the data pointer value
    if !offset > 0 then 
        r @ (sprintf "    add rbx, %i" (!offset * memsize) :: []) ,FlagsUnset 
    else if !offset < 0 then
        r @ (sprintf "    sub rbx, %i" (abs(!offset * memsize)) :: []) ,FlagsUnset
    else (r, !cleanfinish)
///this function generates slower assembler - only used in output blocks as order matters
let makeasm prog stat=
    prog
    |>List.map     
       (function
        |Incdata(t) ->
            sprintf "    add rbx, %i" (t*memsize)
        |Decdata(t) ->
            sprintf "    sub rbx, %i" (t*memsize)
        |Incbyte(t) ->
            match t with
            |1 -> "    inc QWORD [rbx]"  
            | _ -> sprintf "   add QWORD [rbx], %i " t
        |Decbyte(t) ->
            match t with
            |1 -> "    dec QWORD [rbx]" 
            | _ ->sprintf "   sub QWORD [rbx], %i " t
        |Output -> outstr
        |Rplm |Zero |Lprm |RpRpm |LpLpm -> failwith "optimised instruction not expected - only unoptimised instructions should exist in unoptimised blocks"
        |Whilestart(start,ed) -> //since decbyte, incbyte and lrlm modify the flags registers, we don't need to do the test here
            match stat with
            |FlagsUnset | Wend -> sprintf ws ed start
            |FlagsSet ->
                sprintf "    jz labelend%i
labelstart%i:"   ed start
        |Whileend(start,ed) -> //whileend sequences can occur (once about 1/2 way through program),
                                //if you make it past the first whileend instruction you will always make it past all the rest, so no need for checks.
            match stat with
            |FlagsUnset -> sprintf we start ed
            |FlagsSet -> 
                sprintf "   jnz labelstart%i
labelend%i:"     start ed
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
                let r = makeasm (t::[]) (!prev) 
                match t with
                |Whileend(_) -> prev := Wend
                | _ -> prev := FlagsSet
                r
                    )
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
