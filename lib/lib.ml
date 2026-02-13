type re = Eps | Imp of char | Alt of re * re | Seq of re * re | Rep of re

let rec string_of_re s : string =
  match s with
  | Eps -> "$"
  | Imp f -> Char.escaped f
  | Rep x -> "Rep(" ^ string_of_re x ^ ")"
  | Alt (x, y) -> "Alt(" ^ string_of_re x ^ "|" ^ string_of_re y ^ ")"
  | Seq (x, y) -> "Seq(" ^ string_of_re x ^ ";" ^ string_of_re y ^ ")"

(*************************************************************************)

type branch = char * re

let rec actv s : branch list =
  let seq x y =
    match (x, y) with Eps, _ -> y | _, Eps -> x | _, _ -> Seq (x, y)
  in
  match s with
  | Eps -> [ ('_', Eps) ]
  | Imp f -> [ (f, Eps) ]
  | Rep x ->
      List.flatten
        [
          actv Eps;
          List.map (fun v -> match v with i, k -> (i, seq k s)) (actv x);
        ]
  | Alt (x, y) -> List.flatten [ actv x; actv y ]
  | Seq (x, y) ->
      let zs = actv x in
      let xs = List.map (fun v -> match v with i, k -> (i, seq k y)) zs in
      if List.mem ('_', Eps) zs then List.flatten [ xs; actv y ] else xs

(*************************************************************************)

module CharMap = Map.Make (Char)

let f = Failure "fail"

type possibly_many = One of int | Two of (int * int)
type 'a arity = Id | Arity1 of (int -> 'a) | Arity2 of (int * int -> 'a)
type fn = possibly_many arity
type pd = bool arity

let imps : fn CharMap.t =
  CharMap.(
    empty
    |> add 'A' (Arity1 (fun n -> Two (1, n)))
    |> add 'B' (Arity2 (fun (a, n) -> Two (a * n, n - 1)))
    |> add 'C' (Arity2 (fun (a, _) -> One a))
    |> add 'D' (Arity2 (fun (n, m) -> Two (n - m, m)))
    |> add 'E' (Arity2 (fun (n, m) -> Two (n, m - n)))
    |> add 'F' (Arity2 (fun (n, _) -> One n))
    |> add '_' Id)

let doms : pd CharMap.t =
  CharMap.(
    empty
    |> add 'A' (Arity1 (fun _ -> true))
    |> add 'B' (Arity2 (fun (_, _) -> true))
    |> add 'C' (Arity2 (fun (_, n) -> n = 0))
    |> add 'D' (Arity2 (fun (n, m) -> n >= m))
    |> add 'E' (Arity2 (fun (n, m) -> m >= n))
    |> add 'F' (Arity2 (fun (n, m) -> n = m))
    |> add '_' Id)

module StringSet = Set.Make (String)

let pribasis : StringSet.t CharMap.t =
  CharMap.(
    empty
    |> add 'A' @@ StringSet.of_list []
    |> add 'B' @@ StringSet.of_list [ "0<=n" ]
    |> add 'C' @@ StringSet.of_list [ "0<=n"; "n<=0" ]
    |> add 'D' @@ StringSet.of_list [ "m<=n" ]
    |> add 'E' @@ StringSet.of_list [ "n<=m" ]
    |> add 'F' @@ StringSet.of_list [ "m<=n"; "n<=m" ]
    |> add '_' @@ StringSet.of_list [])

let pris : int CharMap.t =
  let calcpri s =
    CharMap.bindings pribasis
    |> List.map (fun (_, v) -> Bool.to_int (StringSet.subset s v))
    |> List.fold_left ( + ) 0
  in
  CharMap.map calcpri pribasis

(*************************************************************************)

let sactv s : branch list =
  let scmp x y =
    match (x, y) with (i, _), (j, _) -> CharMap.(find i pris - find j pris)
  in
  List.sort scmp (actv s)

let funcall f s : possibly_many =
  match (f, s) with
  | Id, _ -> s
  | Arity1 f1, One s1 -> f1 s1
  | Arity2 f2, Two s2 -> f2 s2
  | _, _ -> raise (Failure "fail")

let prdcall f s : bool =
  match (f, s) with
  | Id, _ -> true
  | Arity1 f1, One s1 -> f1 s1
  | Arity2 f2, Two s2 -> f2 s2
  | _, _ -> false

(*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *)

let rec runx r s : int =
  match r with
  | Eps -> ( match s with One n -> n | _ -> raise f)
  | _ -> dalts s (sactv r)

and dalts s zs : int =
  match zs with
  | [] -> raise (Failure "not exhaustive")
  | (i, k) :: zs2 ->
      if prdcall (CharMap.find i doms) s then
        runx k (funcall (CharMap.find i imps) s)
      else dalts s zs2

let run s n = runx s (One n)
let run2 s n m = runx s (Two (n, m))
