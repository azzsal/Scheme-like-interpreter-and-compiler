type register = Rax

let string_of_register ?(last_byte = false) (reg : register) =
  match (reg, last_byte) with Rax, false -> "rax" | Rax, true -> "al"

type operand = Reg of register | Imm of int

let is_register o = match o with Reg _ -> true | _ -> false

let string_of_operand ?(last_byte = false) = function
  | Reg r ->
      string_of_register ~last_byte r
  | Imm i ->
      string_of_int i

type directive =
  | Global of string
  | Section of string
  | Label of string
  | Mov of (operand * operand)
  | Add of (operand * operand)
  | Sub of (operand * operand)
  | Shl of (operand * operand)
  | Shr of (operand * operand)
  | Cmp of (operand * operand)
  | And of (operand * operand)
  | Or of (operand * operand)
  | Setz of operand
  | Ret
  | Comment of string

let string_of_directive ~macos = function
  (* frontmatter *)
  | Global l ->
      Printf.sprintf (if macos then "global _%s" else "global %s") l
  | Section l ->
      Printf.sprintf "\tsection .%s" l
  (* labels *)
  | Label l ->
      Printf.sprintf (if macos then "_%s:" else "%s:") l
  (* instructions *)
  | Mov (dest, src) ->
      Printf.sprintf "\tmov %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Add (dest, src) ->
      Printf.sprintf "\tadd %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Sub (dest, src) ->
      Printf.sprintf "\tsub %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Shl (dest, src) ->
      Printf.sprintf "\tshl %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Shr (dest, src) ->
      Printf.sprintf "\tshr %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Cmp (dest, src) ->
      Printf.sprintf "\tcmp %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | And (dest, src) ->
      Printf.sprintf "\tand %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Or (dest, src) ->
      Printf.sprintf "\tor %s, %s" (string_of_operand dest)
        (string_of_operand src)
  | Setz dest ->
      Printf.sprintf "\tsetz %s" (string_of_operand ~last_byte:true dest)
  | Ret ->
      "\tret"
  | Comment s ->
      Printf.sprintf "; %s" s
