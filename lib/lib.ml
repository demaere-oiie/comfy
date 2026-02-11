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
