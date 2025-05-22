open Mallee

let failures = ref 0

let report name ok =
  if not ok then begin
    incr failures;
    Printf.printf "FAIL: %s\n" name
  end

let run src = Check.run_line Check.initial_env src

let expect_type src expected =
  match run src with
  | Ok (_, output) -> report src (output = "- : " ^ expected)
  | Error message ->
      Printf.printf "unexpected error for %s: %s\n" src message;
      report src false

let expect_error src prefix =
  match run src with
  | Ok (_, output) ->
      Printf.printf "expected an error for %s but got %s\n" src output;
      report src false
  | Error message ->
      let n = String.length prefix in
      report src (String.length message >= n && String.sub message 0 n = prefix)

let () =
  expect_type "1" "int";
  expect_type "true" "bool";
  expect_type "-5" "int";
  expect_type "fun x -> x" "'a -> 'a";
  expect_type "fun f x -> f (f x)" "('a -> 'a) -> 'a -> 'a";
  expect_type "fun f g x -> f (g x)" "('a -> 'b) -> ('c -> 'a) -> 'c -> 'b";
  expect_type "fun x y -> x" "'a -> 'b -> 'a";
  expect_type "add 1 2" "int";
  expect_type "lt 1 2" "bool";
  expect_type "if lt 1 2 then 10 else 20" "int";
  expect_type "let x = 5 in add x 1" "int";
  expect_type "let f x y = add x y in f 1" "int -> int";
  expect_type "(fun x -> x) 3" "int";
  expect_type "let id = fun x -> x in if id true then id 1 else id 2" "int";
  expect_type "let pair_up = fun x -> fun f -> f x x in pair_up 1 add" "int";
  expect_type "let const x y = x in const 1 true" "int";
  expect_type "fun x -> let y = x in y" "'a -> 'a";
  expect_type "fun x -> let g = fun y -> x in g" "'a -> 'b -> 'a";
  expect_type "fun p -> if p then 1 else 0" "bool -> int";
  expect_error "fun f -> if f true then f 1 else 2" "type error";
  expect_error "fun x -> x x" "type error: cannot construct the infinite type";
  expect_error "if 1 then 2 else 3" "type error: cannot unify";
  expect_error "if true then 1 else false" "type error";
  expect_error "add 1 true" "type error";
  expect_error "1 2" "type error";
  expect_error "y" "type error: unbound variable y";
  expect_error "1 $ 2" "lexical error at column 3";
  expect_error "(1" "syntax error: expected ')'";
  expect_error "if true then 1" "syntax error";
  expect_error "fun -> 1" "syntax error";
  expect_error "99999999999999999999" "lexical error"

let () =
  let env = Check.initial_env in
  match Check.run_line env "let k x y = x" with
  | Error message ->
      print_endline message;
      report "definition" false
  | Ok (env, output) -> (
      report "definition output" (output = "val k : 'a -> 'b -> 'a");
      match Check.run_line env "k 1 true" with
      | Ok (_, result) -> report "definition is polymorphic" (result = "- : int")
      | Error _ -> report "definition is polymorphic" false)

let () =
  match Check.run_line Check.initial_env "let x = 1" with
  | Ok (_, output) -> report "toplevel definition" (output = "val x : int")
  | Error _ -> report "toplevel definition" false

let () =
  if !failures > 0 then begin
    Printf.printf "%d failures\n" !failures;
    exit 1
  end
  else print_endline "all checks passed"
