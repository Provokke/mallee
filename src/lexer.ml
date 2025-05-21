type token =
  | INT of int
  | IDENT of string
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | FUN
  | ARROW
  | EQUALS
  | LPAREN
  | RPAREN
  | EOF

exception Error of int * string

let is_digit c = c >= '0' && c <= '9'

let is_ident_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_ident_char c = is_ident_start c || is_digit c || c = '\''

let keyword = function
  | "let" -> LET
  | "in" -> IN
  | "fun" -> FUN
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | "true" -> TRUE
  | "false" -> FALSE
  | name -> IDENT name

let tokenize src =
  let n = String.length src in
  let scan_while pred start =
    let j = ref start in
    while !j < n && pred src.[!j] do
      incr j
    done;
    !j
  in
  let rec go i acc =
    if i >= n then List.rev (EOF :: acc)
    else
      match src.[i] with
      | ' ' | '\t' | '\n' | '\r' -> go (i + 1) acc
      | '(' -> go (i + 1) (LPAREN :: acc)
      | ')' -> go (i + 1) (RPAREN :: acc)
      | '=' -> go (i + 1) (EQUALS :: acc)
      | '-' when i + 1 < n && src.[i + 1] = '>' -> go (i + 2) (ARROW :: acc)
      | c when is_digit c || (c = '-' && i + 1 < n && is_digit src.[i + 1]) ->
          let j = scan_while is_digit (i + 1) in
          let text = String.sub src i (j - i) in
          (match int_of_string_opt text with
           | Some value -> go j (INT value :: acc)
           | None -> raise (Error (i, "integer literal out of range: " ^ text)))
      | c when is_ident_start c ->
          let j = scan_while is_ident_char (i + 1) in
          go j (keyword (String.sub src i (j - i)) :: acc)
      | c -> raise (Error (i, Printf.sprintf "unexpected character '%c'" c))
  in
  go 0 []
