open Lexer

exception Error of string

let describe = function
  | INT n -> string_of_int n
  | IDENT name -> "'" ^ name ^ "'"
  | TRUE -> "'true'"
  | FALSE -> "'false'"
  | IF -> "'if'"
  | THEN -> "'then'"
  | ELSE -> "'else'"
  | LET -> "'let'"
  | IN -> "'in'"
  | FUN -> "'fun'"
  | ARROW -> "'->'"
  | EQUALS -> "'='"
  | LPAREN -> "'('"
  | RPAREN -> "')'"
  | EOF -> "end of input"

let starts_atom = function
  | INT _ | IDENT _ | TRUE | FALSE | LPAREN -> true
  | _ -> false

let parse_tokens tokens =
  let rest = ref tokens in
  let peek () = match !rest with token :: _ -> token | [] -> EOF in
  let advance () = match !rest with _ :: tl -> rest := tl | [] -> () in
  let fail message = raise (Error message) in
  let expect token what =
    if peek () = token then advance ()
    else fail (Printf.sprintf "expected %s but found %s" what (describe (peek ())))
  in
  let ident () =
    match peek () with
    | IDENT name ->
        advance ();
        name
    | token -> fail (Printf.sprintf "expected a name but found %s" (describe token))
  in
  let rec params () =
    match peek () with
    | IDENT name ->
        advance ();
        let more = params () in
        name :: more
    | _ -> []
  in
  let abstract names body = List.fold_right (fun name inner -> Ast.Fun (name, inner)) names body in
  let rec expr () =
    match peek () with
    | LET ->
        let name, bound = binding () in
        expect IN "'in'";
        let body = expr () in
        Ast.Let (name, bound, body)
    | FUN ->
        advance ();
        let names = params () in
        if names = [] then fail "expected a parameter after 'fun'";
        expect ARROW "'->'";
        abstract names (expr ())
    | IF ->
        advance ();
        let cond = expr () in
        expect THEN "'then'";
        let yes = expr () in
        expect ELSE "'else'";
        let no = expr () in
        Ast.If (cond, yes, no)
    | _ -> application ()
  and binding () =
    expect LET "'let'";
    let name = ident () in
    let names = params () in
    expect EQUALS "'='";
    let bound = expr () in
    (name, abstract names bound)
  and application () =
    let head = atom () in
    apply head
  and apply fn =
    if starts_atom (peek ()) then
      let arg = atom () in
      apply (Ast.App (fn, arg))
    else fn
  and atom () =
    match peek () with
    | INT n ->
        advance ();
        Ast.Int n
    | TRUE ->
        advance ();
        Ast.Bool true
    | FALSE ->
        advance ();
        Ast.Bool false
    | IDENT name ->
        advance ();
        Ast.Var name
    | LPAREN ->
        advance ();
        let inner = expr () in
        expect RPAREN "')'";
        inner
    | token -> fail (Printf.sprintf "unexpected %s" (describe token))
  in
  let finish result =
    expect EOF "end of input";
    result
  in
  match peek () with
  | LET -> (
      let name, bound = binding () in
      match peek () with
      | IN ->
          advance ();
          let body = expr () in
          finish (Ast.Expr (Ast.Let (name, bound, body)))
      | _ -> finish (Ast.Def (name, bound)))
  | _ -> finish (Ast.Expr (expr ()))

