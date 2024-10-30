open S_exp

(******************************************************************************)
(* Task 1 *)

(* Task 1.1 *)

let rec string_of_s_exp : s_exp -> string = function
  | Num n -> string_of_int n 
  | Sym s -> s 
  | Lst lst -> "(" ^ (lst |> List.map string_of_s_exp |> String.concat " ") ^ ")" 

(******************************************************************************)
(* Task 2 *)

(* Task 2.1 is found in `test/test_arith.ml` *)

(* Task 2.2 *)

let rec is_bin : s_exp -> bool = function 
  | Num _ -> true 
  | Lst [Sym "+" | Sym "*"; s_exp'; s_exp''] -> 
      is_bin s_exp' && is_bin s_exp''
  | _ -> false

(* Task 2.3 is found in `test/test_arith.ml` *)

(* Task 2.4 *)

exception Stuck of s_exp

let rec interp_bin : s_exp -> int =
  fun exp ->
    match exp with 
    | Num n -> n
    | Lst [Sym "+"; s_exp'; s_exp''] -> 
        interp_bin s_exp' + interp_bin s_exp''
    | Lst [Sym "*"; s_exp'; s_exp''] -> 
        interp_bin s_exp' * interp_bin s_exp''
    | _ -> raise (Stuck exp)

(* Task 2.5 is found in `test/test_arith.ml` *)

(* Task 2.6 *)

type instr
  = Push of int
  | Add
  | Mul

type stack =
  int list

exception ShortStack of stack

let interp_instr : stack -> instr -> stack =
  fun stack instr ->
    match stack, instr with 
    | _ , Push n ->
      n :: stack 
    | first :: second :: rest, Add ->
      (first + second) :: rest
    | first :: second :: rest, Mul -> 
      (first * second) :: rest
    | _ -> raise (ShortStack stack)

let rec interp_instrs : stack -> instr list -> int =
  fun stack instrs -> 
    match instrs with 
    | [] -> 
        List.hd stack
    | first_instr :: rest_instrs -> 
        let updated_stack = interp_instr stack first_instr in 
        interp_instrs updated_stack rest_instrs


let interp_program : instr list -> int =
  fun instrs -> 
    interp_instrs [] instrs
  

(* Task 2.7 is found in `test/test_arith.ml` *)

(* Task 2.8 *)

let rec compile_bin : s_exp -> instr list =
  fun exp ->
    match exp with 
    | Num n -> [Push n]
    | Lst [Sym "+"; e1; e2] -> 
      compile_bin e1 @
      compile_bin e2 @ 
      [Add]
    | Lst [Sym "*"; e1; e2] -> 
      compile_bin e1 @ 
      compile_bin e2 @ 
      [Mul]
    | _ -> raise (Stuck exp)

(* Task 2.9 is found in `test/test_arith.ml` *)

(******************************************************************************)
(* Task 3 *)

(* Task 3.1 *)

let rec desugar_variadic : s_exp -> s_exp =
  fun exp ->
    match exp with 
    | Num n -> Num n 
    | Lst [Sym op] when op = "+" || op = "*" ->
        let n = if op = "+" then 0 else 1 in 
        Lst [Sym op; Num n; Num n]
    | Lst [Sym op; e1] when op = "+" || op = "*"  -> 
        let n = if op = "+" then 0 else 1 in 
        Lst [Sym op; Num n; desugar_variadic e1]
    | Lst [Sym op; e1; e2] when op = "+" || op = "*" ->
        Lst [Sym op; desugar_variadic e1; desugar_variadic e2]
    | Lst (Sym op :: rest) when op = "+" || op = "*" ->
        Lst [Sym op; desugar_variadic (List.hd rest);
         desugar_variadic (Lst (Sym op :: List.tl rest))]
    | _ -> raise (Stuck exp)


(* Task 3.2 *)

let rec interp_variadic : s_exp -> int =
  fun exp ->
    match exp with 
    | Num n -> n 
    | Lst (Sym "+" :: rest) ->
       rest |> List.map interp_variadic |> List.fold_left ( + ) 0
    | Lst (Sym "*" :: rest) ->
       rest |> List.map interp_variadic |> List.fold_left ( * ) 1
    | _ -> raise (Stuck exp)

(* Task 3.3 is found in `test/test_arith.ml` *)
