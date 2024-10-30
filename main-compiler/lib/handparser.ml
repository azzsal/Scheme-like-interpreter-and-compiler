type s_exp = Num of int | Sym of string | Lst of s_exp list 

type token = NUM of int | SYM of string | LPAREN | RPAREN 

exception ParseError

let chunkify (s: string) : string list = 
  s |> Str.global_replace (Str.regexp "(") " ( "
    |> Str.global_replace (Str.regexp ")") " ) "
    |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")

let token_of_string (s: string) : token = 
  match s with 
  | "(" -> 
      LPAREN
  | ")" -> 
      RPAREN
  | _ when Str.string_match (Str.regexp "-?[0-9]+") s 0 
      && String.length (Str.matched_string s) = String.length s -> 
      NUM (int_of_string s)
  | _ when Str.string_match (Str.regexp "[-+_a-zA-z]+") s 0 
      && String.length (Str.matched_string s) = String.length s-> 
      SYM s 
  | _ -> failwith ("Invaild token: " ^ s)          

let tokenize (s: string) : token list = 
  s |> chunkify |> List.map token_of_string
  
let rec parse_s_exp (toks: token list) : s_exp * token list = 
  match toks with 
  | NUM n :: toks' -> 
      (Num n, toks')
  | SYM s :: toks' -> 
      (Sym s, toks')
  | LPAREN :: toks' -> 
      let s_exps, toks'' = parse_lst toks' in 
      (Lst s_exps, toks'')
  | _ -> raise ParseError

and parse_lst (toks: token list) : s_exp list * token list = 
  match toks with
  | RPAREN :: toks' -> 
      ([], toks')
  | _ -> 
      let s_exp, toks' = parse_s_exp toks in 
      let s_exps, toks'' = parse_lst toks' in 
      (s_exp :: s_exps, toks'')

let parse (program : string) : s_exp = 
  let toks = tokenize program in 
  let s_exp, toks = parse_s_exp toks in 
  if List.length toks = 0 then s_exp else raise ParseError  