open Printf
type action = Ast | Compile | Execute 

let target_file = "MatCab"

let _ = 
	let action = if Array.length Sys.argv > 1 then
		List.assoc Sys.argv.(1) [ ("-a", Ast); ("-c",Compile); ("-e", Execute) ]
	else Compile in
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.program Scanner.token lexbuf in
	match action with
		Ast -> 	let listing = Ast.string_of_program program in print_string listing
	    |   Compile -> let listing2 = Compile.translate program in print_string listing2
	    |	Execute -> let code = Compile.translate program 
			and oc = open_out (target_file ^ ".java") in
			fprintf oc "%s" code;
			close_out oc; 
			ignore (Execute.rock target_file)
