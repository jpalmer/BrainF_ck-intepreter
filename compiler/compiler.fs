module Main
//some comments on the generated asm
//mov doesn't effect the flags register.  Canonical way of getting flags is to use and rax,rax
//An idea for the future - there are some palces where we get

//inc [rbx+offset]
//add rbx,offset
//mov rax,[rbx]
//and rax,rax
//jz .....

//this could be faster - avoid a mov (may be slower due to dependencies but I doubt it)if instead it was 
//add rbx,offset
//inc[rbx]
//jz

//there is also some brain dead asm before my optimised blocks
//I suspect I now need an asm level optimiser
type Register =
    |Rax |Rbx |Vd
    with member x.ASM = match x with |Rax -> "rax" |Rbx -> "rbx" |Vd -> "vd"
type Location = 
    |Reg of Register //the value in a register
    |Memloc of Register //the value in [register]
    |Memoffset of Register * int
    with member x.ASM = match x with |Reg(r) -> r.ASM |Memloc(r) -> sprintf "[%s]" (r.ASM) |Memoffset(r,i) -> sprintf "[%s%+i]" (r.ASM) i
type Value = 
    |Loc of Location
    |I of int
    with member x.ASM = match x with |Loc(l) -> l.ASM |I(i) -> sprintf "%i" i
type Instruction = 
    |Push of Register
    |Mov of Location * Value
    |Pop of Register
    |And of Register
    |Jz of string |Jnz of string
    |Inc of Location
    |Dec of Location
    |Add of Location * Value
    |Sub of Location * Value
    |Call of string
    |Label of string
    |Shl of Register * int
    member x.ASM =
        match x with
        |Push(r) -> sprintf "push QWORD %s" (r.ASM)
        |Mov(a,b) -> sprintf "mov QWORD %s,%s" (a.ASM) (b.ASM)
        |Pop(r) -> sprintf "pop QWORD %s" (r.ASM)
        |And(r) -> sprintf "and QWORD %s,%s" (r.ASM) (r.ASM)
        |Jz(s) -> sprintf "jz %s" s
        |Jnz(s) -> sprintf "jnz %s" s
        |Inc(l) -> sprintf "inc QWORD %s" (l.ASM)
        |Dec(l) -> sprintf "dec QWORD %s" (l.ASM)
        |Add(l,v) -> sprintf "add QWORD %s,%s" (l.ASM) (v.ASM)
        |Sub(l,v) -> sprintf "sub QWORD %s,%s" (l.ASM) (v.ASM)
        |Call(s) -> sprintf "call %s" s
        |Label(s) -> sprintf "%s:" s
        |Shl(r,c) -> sprintf "shl %s, %i" (r.ASM) c
open Initial
//the strings below make the code that uses them a bit ugly
let outstr = Push(Rbx)::Mov(Reg(Vd),Loc(Memloc(Rbx)))::Call("putchar")::Pop(Rbx)::[] 
    //unfortunately the compiler barfs on externally defined printf format strings
let ws i j= 
    Mov(Reg(Rax),Loc(Memloc(Rbx)))::And(Rax)::Jz(sprintf "labelend%i" i)::Label(sprintf "labelstart%i" j)::[]
let we i j= 
    Mov(Reg(Rax),Loc(Memloc(Rbx)))::And(Rax)::Jnz(sprintf "labelstart%i" i)::Label(sprintf "labelend%i" j)::[]
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
        |>List.collect
            (fun t ->
                match t with
                |Incdata(t) -> offset := !offset + t; cleanfinish := FlagsUnset; []
                |Decdata(t) -> offset := !offset - t; cleanfinish := FlagsUnset; []
                |Incbyte(t) -> 
                    cleanfinish := FlagsSet; 
                    if t = 1 then Inc(Memoffset(Rbx,!offset * memsize))::[] 
                    else  Add(Memoffset(Rbx,!offset * memsize),I(t))::[]
                |Decbyte(t) -> 
                    cleanfinish := FlagsSet; 
                    if t = 1 then Dec(Memoffset(Rbx,!offset * memsize))::[]
                    else Sub(Memoffset(Rbx,!offset * memsize),I(t))::[]
                |Zero -> 
                    cleanfinish := FlagsUnset
                    Mov(Memloc(Rbx),I(0))::[]
                |Rppm -> //performs a multiply by 2 and adds
                    cleanfinish := FlagsUnset
                    Mov(Reg(Rax),Loc(Memloc(Rbx)))::Shl(Rax,1)::Add(Memoffset(Rbx,8),Loc(Reg(Rax)))::Mov(Memloc(Rbx),I(0))::[]
                |Rplm ->
                    cleanfinish := FlagsUnset
                    Mov(Reg(Rax),Loc(Memloc(Rbx)))::Add(Memoffset(Rbx,8),Loc(Reg(Rax)))::Mov(Memloc(Rbx),I(0))::[]
                |Lprm ->
                    cleanfinish := FlagsUnset
                    Mov(Reg(Rax),Loc(Memloc(Rbx)))::Add(Memoffset(Rbx,-8),Loc(Reg(Rax)))::Mov(Memloc(Rbx),I(0))::[]
                |RpRpm ->
                    cleanfinish := FlagsUnset
                    Mov(Reg(Rax),Loc(Memloc(Rbx)))::Add(Memoffset(Rbx,8),Loc(Reg(Rax)))::Add(Memoffset(Rbx,16),Loc(Reg(Rax)))::Mov(Memloc(Rbx),I(0))::[]
                |LpLpm ->
                    cleanfinish := FlagsUnset
                    Mov(Reg(Rax),Loc(Memloc(Rbx)))::Add(Memoffset(Rbx,-8),Loc(Reg(Rax)))::Add(Memoffset(Rbx,-16),Loc(Reg(Rax)))::Mov(Memloc(Rbx),I(0))::[]
                |Output |Whilestart(_) |Whileend(_) -> failwith "invalid instruction in compute block"
                )
    //now actually change the data pointer value
    if !offset > 0 then 
        r @ ((Add(Reg(Rbx),I(!offset*memsize))) :: []) ,FlagsUnset 
    else if !offset < 0 then
        r @ ((Sub(Reg(Rbx),I(abs(!offset*memsize)))) :: []) ,FlagsUnset 
    else (r, !cleanfinish)
///this function generates slower assembler - only used in output blocks as order matters
let makeasm prog stat=
    prog
    |>List.collect
       (function
        |Incdata(t) ->
            Add(Reg(Rbx),I(t*memsize))::[]
        |Decdata(t) ->
            Sub(Reg(Rbx),I(t*memsize))::[]
        |Incbyte(t) ->
            match t with
            |1 -> Inc(Memloc(Rbx)) ::[]
            | _ -> Add(Memloc(Rbx),I(t))::[]
        |Decbyte(t) ->
            match t with
            |1 -> Dec(Memloc(Rbx))::[]
            | _ -> Sub(Memloc(Rbx),I(t))::[]
        |Output -> outstr
        |Rplm |Zero |Lprm |RpRpm |LpLpm -> failwith "optimised instruction not expected - only unoptimised instructions should exist in unoptimised blocks"
        |Whilestart(start,ed) -> //since decbyte, incbyte and lrlm modify the flags registers, we don't need to do the test here
            match stat with
            |FlagsUnset | Wend -> ws ed start
            |FlagsSet ->
                Jz(sprintf "labelend%i" ed)::Label(sprintf "labelstart%i" start)::[]
        |Whileend(start,ed) -> //whileend sequences can occur (once about 1/2 way through program),
                                //if you make it past the first whileend instruction you will always make it past all the rest, so no need for checks.
            match stat with
            |FlagsUnset -> we start ed
            |FlagsSet -> 
                Jnz(sprintf"labelstart%i" start)::Label(sprintf"labelend%i" ed)::[]
            |Wend -> Label(sprintf "labelend%i" ed)::[]
        )
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
let inline dump arr= Array.map (fun t -> eprintfn "%A   " t;t) arr
printfn "%s" header
program |> tokenize |> optimize |> dump |> fixwhiles |> Blockify |> compileComplete |> List.iter (fun t -> printfn "%s" t.ASM)
let footer = "xor rax,rax ;return 0
	leave
    ret"
printfn "%s" footer
