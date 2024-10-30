open OUnit2
open S_exp
open Lib.Arith

(******************************************************************************)
(* Tasks *)

(* Task 2.1 *)

let test_is_bin : test_ctxt -> unit =
  fun _ ->
    List.iter 
    ( fun (input, expected) ->
        assert_equal 
        ~printer: string_of_bool
        expected 
        (is_bin input)
    )
    [
      (Num 4, true);
      (Lst [Sym "+"; Num 4; Num 6;], true);
      (Lst [Sym "*"; Num 4; Num 5;], true);
      (Lst [Sym "+"; Lst [Sym "*"; Lst [Sym "*"; Num 4; Num 5;]; Num 42]; Num 9], true);
      (Sym "hi", false);
      (Sym "+", false);
      (Lst [], false);
      (Lst [Lst [Num 42]], false)
    ]

(* Task 2.3 *)

let test_interp_bin : test_ctxt -> unit =
  fun _ ->
    (List.iter 
    (
      fun (input, expected) -> 
        assert_raises expected (fun _ -> interp_bin input)
    )
    [
      (Sym "hi", Stuck (Sym "hi"));
      (Sym "+", Stuck (Sym "+"));
      (Lst [], Stuck (Lst []));
      (Lst [Lst [Num 42]], Stuck (Lst [Lst [Num 42]]));
      (parse "(+ 42 (3))", Stuck (parse "(3)"))
    ]);
    (List.iter 
    (
      fun (input, expected) -> 
        assert_equal ~printer: string_of_int expected (interp_bin input)

    )
    [
      (Num 4, 4);
      (Lst [Sym "+"; Num 4; Num 6;], 10);
      (Lst [Sym "*"; Num 4; Num 5;], 20);
      (Lst [Sym "+"; Lst [Sym "*"; Lst [Sym "*"; Num 4; Num 5;]; Num 42]; Num 9], 849);
    ])

(* Task 2.5 *)

let show_stack : stack -> string = 
  fun stack ->
    "[" ^ (stack |> List.map string_of_int |> String.concat " ") ^ "]"
  

let test_interp_instr : test_ctxt -> unit =
  fun _ ->
    (List.iter 
    (
      fun (st, inst, expected) -> 
        assert_equal ~printer: show_stack expected (interp_instr st inst)
    )
    [
        ([], Push 5, [5]);
        ([5; 7], Push 3, [3; 5; 7]);
        ([3; 5], Add, [8]);
        ([3; 5], Mul, [15])
    ]);
    (List.iter 
    (
      fun (st, inst, expected) ->
        assert_raises expected (fun _ -> interp_instr st inst) 
    ))
    [
      ([], Add, ShortStack []);
      ([1], Add, ShortStack [1]);
    ]

let test_interp_program : test_ctxt -> unit =
  fun _ ->
    (List.iter 
    (
      fun (instr_list, expected) -> 
        assert_equal ~printer: string_of_int expected (interp_program instr_list)
    )
    [
      ([Push 3], 3);
      ([Push 3; Push 4], 4);
      ([Push 3; Push 4; Add], 7);
      ([Push 3; Push 4; Mul], 12);
      ([Push 7; Push 3; Add; Push 2; Mul], 20);
    ]); 
    (List.iter
    (
      fun (instr_list, expected) ->
        assert_raises expected (fun _ -> interp_program instr_list)
    )
    [
      ([Mul], ShortStack []);
      ([Push 3; Add], ShortStack [3]);
    ]
    )
    

(* Task 2.7 *)

let string_of_instr = function 
  | Push n -> "Push " ^ string_of_int n 
  | Add -> "Add"
  | Mul -> "Mul"

let rec show_instrs = function 
  | [] -> ""
  | [instr] -> string_of_instr instr 
  | first_instr :: rest_instrs -> string_of_instr first_instr ^ "\n" ^ show_instrs rest_instrs 

let test_compile_bin : test_ctxt -> unit =
  fun _ ->
    (List.iter
    (
      fun (input, expected) -> 
        assert_equal ~printer: show_instrs expected (compile_bin input)
    )
    [
      (Num 4, [Push 4]);
      (Lst [Sym "+"; Num 4; Num 6;], [Push 4; Push 6; Add]);
      (Lst [Sym "*"; Num 4; Num 5;], [Push 4; Push 5; Mul]);
      (Lst [Sym "+"; Lst [Sym "*"; Lst [Sym "*"; Num 4; Num 5;]; Num 42]; Num 9],
      [
        Push 4;
        Push 5;
        Mul;
        Push 42;
        Mul;
        Push 9;
        Add
      ]);
    ]);
    (List.iter 
    (
      fun (input, expected) -> 
        assert_raises expected (fun _ -> compile_bin input)
    )
    [
      (Lst [Sym "+"; Num 4], Stuck (Lst [Sym "+"; Num 4]));
    ])

(* Task 2.9 *)

let test_compile_versus_interp_bin : test_ctxt -> unit =
  fun _ ->
    (List.iter 
    (
      fun (input, expected) -> 
        (assert_equal ~printer: string_of_int expected (interp_bin input));
        assert_equal ~printer: string_of_int expected (interp_program (compile_bin input))
    )
    [
      (Num 4, 4);
      (Lst [Sym "+"; Num 3; Num 2], 5);
      (Lst [Sym "*"; Num 42; Num 2], 84);
      (Lst [Sym "+"; Num 7; Lst [Sym "*"; Num 3; Num 2]], 13)
    ]);
    (List.iter
    (
      fun (input, expected) -> 
        (assert_raises expected (fun _ -> interp_bin input));
        assert_raises expected (fun _ -> interp_program (compile_bin input))
    )
    [
      (Lst [], Stuck (Lst []));
      (Lst [Sym "+"], Stuck (Lst [Sym "+"]));
      (Lst [Sym "+"; Num 4], Stuck (Lst [Sym "+"; Num 4]));
    ])

(* Task 3.3 *)

let test_variadic : test_ctxt -> unit =
  fun _ ->
    (List.iter
    (
      fun (input, expected) ->
        (assert_equal ~printer: string_of_int expected 
        (input |> desugar_variadic |> interp_bin));
        assert_equal ~printer: string_of_int expected (interp_variadic input)
    )
    [
      (parse "(+ 1 (+ 7 8 9) 3)", 28);
      (parse "(+ 1)", 1);
      (parse "(*)", 1);
      (parse "(* 2 3 3 (+ 7))", 126)
    ]);
    (List.iter
    (
      fun (input, expected) ->
        (assert_raises expected (fun _ -> input |> desugar_variadic |> interp_bin));
        (assert_raises expected (fun _ -> interp_variadic input))
    )
    [
      (parse "((1))", Stuck (parse "((1))"));
      (parse "(+ 1 (/ 4 2))", Stuck (parse "(/ 4 2)"))
    ])

(******************************************************************************)
(* Test runner *)

let _ =
  run_test_tt_main
    ( "arith tests" >:::
        [ "is_bin" >:: test_is_bin
        ; "interp_bin" >:: test_interp_bin
        ; "interp_instr" >:: test_interp_instr
        ; "interp_program" >:: test_interp_program
        ; "compile_bin" >:: test_compile_bin
        ; "compiling vs. interpreting" >:: test_compile_versus_interp_bin
        ; "variadic" >:: test_variadic
        ]
    )
