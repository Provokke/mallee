open Infer

let binary result = TArrow (TInt, TArrow (TInt, result))

let initial_env : env =
  List.map
    (fun (name, ty) -> (name, Forall ([], ty)))
    [
      ("add", binary TInt);
      ("sub", binary TInt);
      ("mul", binary TInt);
      ("eq", binary TBool);
      ("lt", binary TBool);
      ("not", TArrow (TBool, TBool));
    ]

let run_line env src =
  try
    match Parser.parse_tokens (Lexer.tokenize src) with
    | Ast.Expr expr ->
        let _, ty = infer env expr in
        Ok (env, "- : " ^ show ty)
    | Ast.Def (name, expr) ->
        let s, ty = infer env expr in
        let scheme = generalise (apply_env s env) ty in
        Ok ((name, scheme) :: env, Printf.sprintf "val %s : %s" name (show_scheme scheme))
  with
  | Lexer.Error (pos, message) -> Error (Printf.sprintf "lexical error at column %d: %s" (pos + 1) message)
  | Parser.Error message -> Error ("syntax error: " ^ message)
  | Type_error message -> Error ("type error: " ^ message)

