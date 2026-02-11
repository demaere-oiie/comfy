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

let imps =
  CharMap.(
    empty
    |> add 'A' (fun v ->
        match v with 1, n, _ -> (2, 1, n) | _, _, _ -> raise f)
    |> add 'B' (fun v ->
        match v with 2, a, n -> (2, a * n, n - 1) | _, _, _ -> raise f)
    |> add 'C' (fun v ->
        match v with 2, a, _ -> (1, a, 0) | _, _, _ -> raise f)
    |> add '_' (fun v -> v))

let doms =
  CharMap.(
    empty
    |> add 'A' (fun v -> match v with 1, _, _ -> true | _, _, _ -> false)
    |> add 'B' (fun v -> match v with 2, _, _ -> true | _, _, _ -> false)
    |> add 'C' (fun v -> match v with 2, _, 0 -> true | _, _, _ -> false)
    |> add '$' (fun v -> match v with 0, 1, 2 -> true | _, _, _ -> false)
      (*TC?*)
    |> add '_' (fun _ -> true))

let pris = CharMap.(empty |> add 'A' 0 |> add 'B' 1 |> add 'C' 0 |> add '_' 3)

let sactv s =
  let scmp x y =
    match x with
    | i, _ -> ( match y with j, _ -> CharMap.(find i pris - find j pris))
  in
  List.sort scmp (actv s)

let rec runx r s =
  match r with Eps -> ( match s with _, n, _ -> n) | _ -> dalts s (sactv r)

and dalts s zs =
  match zs with
  | [] -> raise (Failure "not exhaustive")
  | (i, k) :: zs2 ->
      if (CharMap.find i doms) s then runx k ((CharMap.find i imps) s)
      else dalts s zs2

let run s n = runx s (1, n, 0)
