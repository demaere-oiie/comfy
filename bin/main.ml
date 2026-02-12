open Comfy.Lib

let fac = Seq (Imp 'A', Seq (Rep (Imp 'B'), Imp 'C'))
let gcd = Seq (Rep (Alt (Imp 'D', Imp 'E')), Imp 'F')

(****
let practv s =
  print_endline "---";
  List.iter print_endline
    (List.map
       (fun v -> match v with i, k -> Char.escaped i ^ " " ^ string_of_re k)
       (sactv s))
****)

let () =
  print_endline @@ "fac = " ^ string_of_re fac;
  print_string "fac 7 = ";
  print_endline @@ string_of_int @@ run fac 7;
  print_endline @@ "gcd = " ^ string_of_re gcd;
  print_string "gcd 30 42 = ";
  print_endline @@ string_of_int @@ run2 gcd 30 42
