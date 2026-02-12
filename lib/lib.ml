type re = Eps | Imp of char | Alt of re * re | Seq of re * re | Rep of re

let seq x y = match (x, y) with Eps, _ -> y | _, Eps -> x | _, _ -> Seq (x, y)

let rec actv s =
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

let rec string_of_re s =
  match s with
  | Eps -> "$"
  | Imp f -> Char.escaped f
  | Rep x -> "Rep(" ^ string_of_re x ^ ")"
  | Alt (x, y) -> "Alt(" ^ string_of_re x ^ "|" ^ string_of_re y ^ ")"
  | Seq (x, y) -> "Seq(" ^ string_of_re x ^ ";" ^ string_of_re y ^ ")"

module CharMap = Map.Make (Char)

let f = Failure "fail"

type possibly_many = One of int | Two of (int * int)
type 'a arity = Id | Arity1 of (int -> 'a) | Arity2 of (int * int -> 'a)
type fn = possibly_many arity
type pd = bool arity

let imps =
  CharMap.(
    empty
    |> add 'A' (Arity1 (fun n -> Two (1, n)))
    |> add 'B' (Arity2 (fun (a, n) -> Two (a * n, n - 1)))
    |> add 'C' (Arity2 (fun (a, _) -> One a))
    |> add 'D' (Arity2 (fun (n, m) -> Two (n - m, m)))
    |> add 'E' (Arity2 (fun (n, m) -> Two (n, m - n)))
    |> add 'F' (Arity2 (fun (n, _) -> One n))
    |> add '_' Id)

let doms =
  CharMap.(
    empty
    |> add 'A' (Arity1 (fun _ -> true))
    |> add 'B' (Arity2 (fun (_, _) -> true))
    |> add 'C' (Arity2 (fun (_, n) -> n = 0))
    |> add 'D' (Arity2 (fun (n, m) -> n >= m))
    |> add 'E' (Arity2 (fun (n, m) -> m >= n))
    |> add 'F' (Arity2 (fun (n, m) -> n = m))
    |> add '_' Id)

let pris =
  CharMap.(
    empty |> add 'A' 0 |> add 'B' 1 |> add 'C' 0 |> add 'D' 1 |> add 'E' 1
    |> add 'F' 0 |> add '_' 6)

let sactv s =
  let scmp x y =
    match x with
    | i, _ -> ( match y with j, _ -> CharMap.(find i pris - find j pris))
  in
  List.sort scmp (actv s)

let funcall f s =
  match (f, s) with
  | Id, _ -> s
  | Arity1 f1, One s1 -> f1 s1
  | Arity2 f2, Two s2 -> f2 s2
  | _, _ -> raise (Failure "fail")

let prdcall f s =
  match (f, s) with
  | Id, _ -> true
  | Arity1 f1, One s1 -> f1 s1
  | Arity2 f2, Two s2 -> f2 s2
  | _, _ -> false

let rec runx r s =
  match r with
  | Eps -> ( match s with One n -> n | _ -> raise f)
  | _ -> dalts s (sactv r)

and dalts s zs =
  match zs with
  | [] -> raise (Failure "not exhaustive")
  | (i, k) :: zs2 ->
      if prdcall (CharMap.find i doms) s then
        runx k (funcall (CharMap.find i imps) s)
      else dalts s zs2

let run s n = runx s (One n)
let run2 s n m = runx s (Two (n, m))
