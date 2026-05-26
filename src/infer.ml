module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

type ty =
  | TInt
  | TBool
  | TVar of int
  | TArrow of ty * ty

type scheme = Forall of int list * ty

type subst = ty IntMap.t

type env = (string * scheme) list

exception Type_error of string

let counter = ref 0

let fresh () =
  incr counter;
  TVar !counter

let show_many types =
  let names = Hashtbl.create 8 in
  let name_of id =
    match Hashtbl.find_opt names id with
    | Some name -> name
    | None ->
        let k = Hashtbl.length names in
        let name = if k < 26 then Printf.sprintf "'%c" (Char.chr (97 + k)) else Printf.sprintf "'t%d" k in
        Hashtbl.add names id name;
        name
  in
  let rec go prec = function
    | TInt -> "int"
    | TBool -> "bool"
    | TVar id -> name_of id
    | TArrow (a, b) ->
        let left = go 1 a in
        let right = go 0 b in
        let text = left ^ " -> " ^ right in
        if prec > 0 then "(" ^ text ^ ")" else text
  in
  List.map (go 0) types

let show ty = List.hd (show_many [ ty ])

let show_scheme (Forall (_, ty)) = show ty

let rec ftv_ty = function
  | TInt | TBool -> IntSet.empty
  | TVar v -> IntSet.singleton v
  | TArrow (a, b) -> IntSet.union (ftv_ty a) (ftv_ty b)

let ftv_scheme (Forall (vars, ty)) = IntSet.diff (ftv_ty ty) (IntSet.of_list vars)

let ftv_env env = List.fold_left (fun acc (_, scheme) -> IntSet.union acc (ftv_scheme scheme)) IntSet.empty env

let rec apply (s : subst) = function
  | TVar v as ty -> ( match IntMap.find_opt v s with Some replacement -> replacement | None -> ty)
  | TArrow (a, b) -> TArrow (apply s a, apply s b)
  | ty -> ty

let apply_scheme s (Forall (vars, ty)) =
  let inner = List.fold_left (fun acc v -> IntMap.remove v acc) s vars in
  Forall (vars, apply inner ty)

let apply_env s env = List.map (fun (name, scheme) -> (name, apply_scheme s scheme)) env

let compose (s2 : subst) (s1 : subst) : subst =
  IntMap.union (fun _ first _ -> Some first) (IntMap.map (apply s2) s1) s2

let bind v ty =
  if ty = TVar v then IntMap.empty
  else if IntSet.mem v (ftv_ty ty) then
    match show_many [ TVar v; ty ] with
    | [ a; b ] -> raise (Type_error (Printf.sprintf "cannot construct the infinite type %s = %s" a b))
    | _ -> assert false
  else IntMap.singleton v ty

let rec unify a b : subst =
  match (a, b) with
  | TInt, TInt | TBool, TBool -> IntMap.empty
  | TVar v, ty | ty, TVar v -> bind v ty
  | TArrow (a1, r1), TArrow (a2, r2) ->
      let s1 = unify a1 a2 in
      let s2 = unify (apply s1 r1) (apply s1 r2) in
      compose s2 s1
  | _ -> (
      match show_many [ a; b ] with
      | [ left; right ] -> raise (Type_error (Printf.sprintf "cannot unify %s with %s" left right))
      | _ -> assert false)

let instantiate (Forall (vars, ty)) =
  let s = List.fold_left (fun acc v -> IntMap.add v (fresh ()) acc) IntMap.empty vars in
  apply s ty

let generalise env ty =
  let vars = IntSet.diff (ftv_ty ty) (ftv_env env) in
  Forall (IntSet.elements vars, ty)

let rec infer (env : env) (expr : Ast.expr) : subst * ty =
  match expr with
  | Ast.Int _ -> (IntMap.empty, TInt)
  | Ast.Bool _ -> (IntMap.empty, TBool)
  | Ast.Var name -> (
      match List.assoc_opt name env with
      | Some scheme -> (IntMap.empty, instantiate scheme)
      | None -> raise (Type_error ("unbound variable " ^ name)))
  | Ast.Fun (param, body) ->
      let tv = fresh () in
      let s1, t1 = infer ((param, Forall ([], tv)) :: env) body in
      (s1, TArrow (apply s1 tv, t1))
  | Ast.App (fn, arg) ->
      let tv = fresh () in
      let s1, t1 = infer env fn in
      let s2, t2 = infer (apply_env s1 env) arg in
      let s3 = unify (apply s2 t1) (TArrow (t2, tv)) in
      (compose s3 (compose s2 s1), apply s3 tv)
  | Ast.Let (name, bound, body) ->
      let s1, t1 = infer env bound in
      let env1 = apply_env s1 env in
      let scheme = generalise env1 t1 in
      let s2, t2 = infer ((name, scheme) :: env1) body in
      (compose s2 s1, t2)
  | Ast.If (cond, yes, no) ->
      let s1, tc = infer env cond in
      let s = compose (unify tc TBool) s1 in
      let s2, ty = infer (apply_env s env) yes in
      let s = compose s2 s in
      let s3, tn = infer (apply_env s env) no in
      let s = compose s3 s in
      let s4 = unify (apply s ty) (apply s tn) in
      let s = compose s4 s in
      (s, apply s ty)
