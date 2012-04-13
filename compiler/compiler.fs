module Main
//some comments on the generated asm
//mov doesn't effect the flags register.  Canonical way of getting flags is to use and rax,rax

//r9 is used as a 0 register
type Register =
    |Rax |Rbx |Vd |Rzx |Rten |Releven
    with member x.ASM = match x with |Rax -> "%rax" |Rbx -> "%rbx" |Vd -> "%rdi" |Rzx -> "%r9" |Rten -> "%r10" |Releven -> "%r11"
type Location = 
    |Reg of Register //the value in a register
    |Memloc of Register //the value in [register]
    |Memoffset of Register * int
    with member x.ASM = match x with |Reg(r) -> r.ASM |Memloc(r) -> sprintf "(%s)" (r.ASM) |Memoffset(r,i) -> if i <> 0 then sprintf "%i(%s)" i (r.ASM) else sprintf "(%s)" (r.ASM)
let MemCorr (reg,offset) = 
    match offset with
    |0 -> Memloc(reg)
    |t -> Memoffset(reg,t)
type Value = 
    |Loc of Location
    |I of int
    with member x.ASM = match x with |Loc(l) -> l.ASM |I(i) -> sprintf "$%i" i
type Instruction = 
    |Push of Register
    |Mov of Location * Value //dest,source
    |Pop of Register
    |And of Register |Xor of Register
    |Jz of string |Jnz of string |Jg of string |Jmp of string
    |Inc of Location
    |Dec of Location
    |Add of Location * Value
    |Sub of Location * Value
    |Call of string
    |Label of string
    |Shl of Register * int
    member x.ASM =
        match x with
        |Push(r) ->     sprintf "    pushq %s" (r.ASM)
        |Mov(a,I(0)) -> sprintf "    movq  %%r9,%s" (a.ASM) //I have a 0 register - special case
        |Mov(a,b) ->    sprintf "    movq  %s,%s" (b.ASM) (a.ASM)
        |Pop(r) ->      sprintf "    popq  %s" (r.ASM)
        |And(r) ->      sprintf "    andq  %s,%s" (r.ASM) (r.ASM)
        |Jz(s) ->       sprintf "    jz %s" s
        |Jg(s) ->       sprintf "    jg %s" s
        |Jmp(s) ->      sprintf "    jmp %s" s
        |Jnz(s) ->      sprintf "    jnz %s" s
        |Inc(l) ->      sprintf "    incq  %s" (l.ASM)
        |Dec(l) ->      sprintf "    decq  %s" (l.ASM)
        |Add(l,v) ->    sprintf "    addq  %s,%s" (v.ASM) (l.ASM)
        |Sub(l,v) ->    sprintf "    subq  %s,%s" (v.ASM) (l.ASM)
        |Call(s) ->     sprintf "    call %s" s
        |Label(s) ->    sprintf "%s:" s
        |Shl(r,c) ->    sprintf "    shl $%i, %s" c (r.ASM) 
        |Xor(r) ->      sprintf "    xor %s,%s" (r.ASM) (r.ASM)
open Initial
//the strings below make the code that uses them a bit ugly
let outstr = Push(Rbx)::Mov(Reg(Vd),Loc(Memloc(Rbx)))::Call("putchar")::Pop(Rbx)::Xor(Rzx)::[] 
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
                    if t = 1 then Inc(MemCorr(Rbx,!offset * memsize))::[] 
                    else  Add(MemCorr(Rbx,!offset * memsize),I(t))::[]
                |Decbyte(t) -> 
                    cleanfinish := FlagsSet; 
                    if t = 1 then Dec(MemCorr(Rbx,!offset * memsize))::[]
                    else Sub(MemCorr(Rbx,!offset * memsize),I(t))::[]
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
let rec asmoptimize instrucs =
    match instrucs with
    |Inc(Memoffset(Rbx, q))::Add(Reg(Rbx),I r)::Mov(Reg(Rax),Loc(Memloc(Rbx)))::And(Rax)::t when q=r-> 
        Add(Reg(Rbx),I q)::Inc(Memloc(Rbx))::(asmoptimize t)
    |Mov(Memloc(Rbx),I(0))::Inc(Memloc(Rbx))::t ->
        Mov(Memloc(Rbx),I(1))::(asmoptimize t)
    |Mov(Memloc(Rbx),I(0))::Add(Reg(Rbx),I 8)::Mov(Reg(Rax),Loc(Memloc(Rbx)))::Add(Memoffset(Rbx,-8),Loc(Reg(Rax)))::Add(Memoffset(Rbx,-16),Loc(Reg(Rax)))::Mov(Memloc(Rbx),I 0)::Sub(Reg(Rbx),I 8)::Mov(Reg(Rax),Loc(Memloc(Rbx)))::Add(Memoffset(Rbx,8),Loc(Reg(Rax)))::Mov(Memloc(Rbx),I 0)::Sub(Reg(Rbx),I 8)::Mov(Reg(Rax),Loc(Memloc(Rbx)))::And(Rax)::t ->
        Mov(Reg(Rax),Loc(Memoffset(Rbx,8)))::Mov(Memloc(Rbx),I 0)::Sub(Reg(Rbx),I 8)::Add(Memloc(Rbx),Loc(Reg(Rax)))::(asmoptimize t)
    |Label("labelstart25")::Inc(Memoffset(Rbx,8))::Dec(Memloc(Rbx))::Jz(endlable)::t ->
        let rec findend l = 
            match l with
            |Label(_)::Inc(Memoffset(Rbx,8))::Dec(Memloc(Rbx))::Jz(_)::q -> findend q
            |newl -> newl
        //new instructions
        Mov(Reg(Rax),Loc(Memloc(Rbx)))::Sub(Reg(Rax),I 9)::Jg("morethan10")::Mov(Reg(Rax),Loc(Memloc(Rbx)))::Add(Memoffset(Rbx,8),Loc(Reg(Rax)))::Mov(Memloc(Rbx),I 0)::Jmp("labelend80")::Label("morethan10")::Add(Memoffset(Rbx,8),I 9)::Sub(Memloc(Rbx),I 9)::(asmoptimize(findend t))
    (*labelstart25: ;this is new
    movq  (%rbx),%rax
    subq  $9,%rax
    jg   morethan10
    movq (%rbx),%rax
    addq %rax,8(%rbx)
    movq $0, (%rbx)
    jmp labelend80
morethan10:
    addq $9,8(%rbx)
    subq $9,(%rbx) ;end of new
*)
    |h::t -> h::(asmoptimize t)
    |[] -> []
let program = ">++++++++++>>>+>+[>>>+[-[<<<<<[+<<<<<]>>[[-]>[<<+>+>-]<[>+<-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>>>>+>+<<<<<<-[>+<-]]]]]]]]]]]>[<+>-]+>>>>>]<<<<<[<<<<<]>>>>>>>[>>>>>]++[-<<<<<]>>>>>>-]+>>>>>]<[>++<-]<<<<[<[>+<-]<<<<]>>[->[-]++++++[<++++++++>-]>>>>]<<<<<[<[>+>+<<-]>.<<<<<]>.>>>>]" //currently at 252 lines (including header) (for reference there are 271 brinf*ck instructions
let header = System.IO.File.ReadAllText("header.txt")
let inline dump arr= Array.map (fun t -> eprintfn "%A   " t;t) arr
printfn "%s" header
program |> tokenize |> optimize |> fixwhiles |> Blockify |> compileComplete |> asmoptimize  |> List.iter (fun t -> printfn "%s" t.ASM)
let footer = "xor %rax,%rax 
	leave
    ret"
printfn "%s" footer
