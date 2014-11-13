open Gencpp
open Labels
open Semantic
open Vars

let _ =
let lexbuf = Lexing.from_channel stdin
in let program = Lamepar.prgm Lamescan.token lexbuf
in let vt = new_vartable ()
in let ls = new_labels ()
in let intcode = check_prgm program vt ls
in let cppcode = gencpp_prog intcode vt
in print_endline cppcode
