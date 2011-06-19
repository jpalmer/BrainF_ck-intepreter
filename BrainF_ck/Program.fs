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
    |Whilestart of int
    |Whileend of int
    |Blank
    static member fromstring x=
        match x with
        |'>' -> Incdata dval
        |'<' -> Decdata dval
        |'+' -> Incbyte dval
        |'-' -> Decbyte dval
        |'.' -> Output
        |',' -> Input
        |'[' -> Whilestart 0
        |']' -> Whileend 0
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
            program.[v] <- Whilestart(curpointer)
            program.[curpointer] <- Whileend(v)
        |_ -> () //do nothing for other stuff
    program


let rec runprogram (program:chars[]) =
    let memory = Array.create 10000 0
    let count = ref 0
    let rec rfun p ip= 
        count := !count + 1
        if ip>= program.Length then () //|| !count > 90000000 then ()
        else
            match program.[ip] with
            |Incdata(t) -> rfun  (p+t) (ip+1)
            |Decdata(t) -> rfun (p-t) (ip+1)
            |Incbyte(t) -> 
                memory.[p] <- memory.[p]+t
                rfun p (ip+1)    
            |Decbyte(t) -> 
                memory.[p] <- memory.[p]-t
                rfun p (ip+1)
            |Output ->
                printf "%c" (memory.[p] |> char)
                rfun p (ip+1)
            |Input ->
                memory.[p] <- System.Console.Read()
                rfun p (ip+1)
            |Whilestart(t) ->
                if memory.[p]=0 then
                    rfun p (t+1)
                else
                    rfun p (ip+1)
            |Whileend(t) ->
                if memory.[p]<>0 then
                    rfun p (t+1)
                else
                    rfun p (ip+1)
            | _ -> failwith "shouldn't happen"
    rfun 0 0
let hello = @">++++++++++>>>+>+[>>>+[-[<<<<<[+<<<<<]>>[[-]>[<<+>+>-]<[>+<-]<[>+<-[>+<-[>
+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>>>>+>+<<<<<<-[>+<-]]]]]]]]]]]>[<+>-
]+>>>>>]<<<<<[<<<<<]>>>>>>>[>>>>>]++[-<<<<<]>>>>>>-]+>>>>>]<[>++<-]<<<<[<[
>+<-]<<<<]>>[->[-]++++++[<++++++++>-]>>>>]<<<<<[<[>+>+<<-]>.<<<<<]>.>>>>]"
hello 
|> tokenize 
|> fun t -> if opt then optimize t else t
|> fixwhiles 
|> fun p -> runprogram p

//System.Console.Read() |> ignore