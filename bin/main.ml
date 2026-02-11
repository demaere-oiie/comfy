open Comfy.Lib

let r = Seq (Imp 'A', Seq (Rep (Imp 'B'), Imp 'C'))
let s = Seq (Rep (Seq (Imp 'A', Imp 'B')), Imp 'C')

let () =
  print_endline @@ string_of_re r;
  List.iter print_endline
    (List.map
       (fun v -> match v with i, k -> Char.escaped i ^ " " ^ string_of_re k)
       (actv r));
  List.iter print_endline
    (List.map
       (fun v -> match v with i, k -> Char.escaped i ^ " " ^ string_of_re k)
       (actv s))
