open Comfy.Lib

let r = Seq (Imp 'A', Seq (Rep (Imp 'B'), Imp 'C'))

(****
let s = Seq (Rep (Seq (Imp 'A', Imp 'B')), Imp 'C')
let t = Seq (Rep (Imp 'B'), Imp 'C')

let practv s =
  print_endline "---";
  List.iter print_endline
    (List.map
       (fun v -> match v with i, k -> Char.escaped i ^ " " ^ string_of_re k)
       (sactv s))
****)

let g = Seq (Rep (Alt (Imp 'D', Imp 'E')), Imp 'F')

let () =
  print_endline @@ string_of_re r;
  print_endline @@ string_of_int @@ run r 7;
  print_endline @@ string_of_re g;
  print_endline @@ string_of_int @@ run2 g 30 42
