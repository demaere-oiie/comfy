open Comfy.Lib

let fac = Seq (Imp 'A', Seq (Rep (Imp 'B'), Imp 'C'))
let gcd = Seq (Rep (Alt (Imp 'D', Imp 'E')), Imp 'F')
let fac2 = Seq (Imp 'A', Rep (Alt (Imp 'B', Imp 'C')))
let gcd2 = Rep (Alt (Imp 'D', Alt (Imp 'E', Imp 'F')))

let () =
  let doit nm re nma arg =
    print_endline @@ nm ^ " = " ^ string_of_re re;
    print_string @@ nma ^ " = ";
    print_endline @@ string_of_int @@ arg
  in
  doit "fac" fac "fac 7" (run fac 7);
  doit "fac" fac2 "fac 7" (run fac2 7);
  doit "gcd" gcd "gcd 30 42" (run2 gcd 30 42);
  doit "gcd" gcd2 "gcd 30 42" (run2 gcd2 30 42)
