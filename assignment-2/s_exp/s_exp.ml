type s_exp = Exp.t

let show = Exp.show

let parse = Parser.parse

let parse_file = Parser.parse_file

let rec string_of_s_exp : s_exp -> string =
  fun e ->
    begin match e with
      | Sym x ->
          x

      | Num n ->
          string_of_int n

      | Lst exps ->
          let exps_string = exps |> List.map string_of_s_exp in
          "(" ^ String.concat " " exps_string ^ ")"

      | Chr c ->
          let after =
            begin match c with
              | '\n' ->
                  "newline"

              | ' ' ->
                  "space"

              | _ ->
                  String.make 1 c
            end
          in
          "#\\" ^ after
    end
