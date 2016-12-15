open System

type Cpu = { pc: int; reg: int array; code: string array }

let (|Reg|Imm|) (s: string) =
    let r = "abcd".IndexOf(s.[0])
    in if r = -1 then
         Imm (Int32.Parse(s))
       else
         Reg r

let (|V|) (cpu: Cpu) (s: string) =
  match s with
  | Reg r -> V (s, cpu.reg.[r])
  | Imm v -> V (s, v)

let testInput =
  [|
    "cpy 41 a"
    "inc a"
    "inc a"
    "dec a"
    "jnz a 2"
    "dec a"
  |]

let cpu0 code = { pc = 0; reg = Array.zeroCreate 4; code = code }

let (|Cont|Done|) cpu =
    if cpu.pc < Array.length cpu.code then
      Cont cpu
    else
      Done cpu

let words (s: string) = s.Split [|' '|]

let set arr i v =
    let arr2 = Array.copy arr
    Array.set arr2 i v
    arr2

let step (Cont cpu) =
    match (words cpu.code.[cpu.pc]) with
    | [|"cpy"; V cpu (_, x); Reg y|] ->
        {cpu with reg = set cpu.reg y x; pc = cpu.pc + 1}
    | [|"inc"; V cpu ((Reg x), v)|] ->
        {cpu with reg = set cpu.reg x (v + 1); pc = cpu.pc + 1}
    | [|"dec"; V cpu ((Reg x), v)|] ->
        {cpu with reg = set cpu.reg x (v - 1); pc = cpu.pc + 1}
    | [|"jnz"; V cpu (_, 0); _|] ->
        {cpu with pc = cpu.pc + 1}
    | [|"jnz"; _; Imm y|] ->
        {cpu with pc = cpu.pc + y}

let rec run =
  function
  | Done cpu -> cpu
  | Cont cpu -> run (step cpu)

let cpu1 code = { pc = 0; reg = set (Array.zeroCreate 4) 2 1; code = code }

run (cpu1 (System.IO.File.ReadAllLines("input.txt")))
|> printfn "%A"
