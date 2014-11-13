{
open Printf
exception Eof
}

let newline    = '\n' | "\r\n"
let whitespace = [' ' '\t']
let digit      = ['0'-'9']
let integer    = digit+
let letter     = ['_' 'a'-'z' 'A'-'Z']
let letdig     = letter | digit
let identifier = letter letdig*
let exp = 'e' ['-' '+']? digit+
let float1 = digit+ '.' digit* exp?
let float2 = digit+ exp
let float3 = '.' digit+ exp?


rule token = parse
  newline				{ Lexing.new_line lexbuf; token lexbuf }
  | whitespace			{ token lexbuf }
  
  (* compute_operators *)
  | '+'					
  | '-'						
  | '*'					
  | '/'	as op1	{ printf "Operator 1 %c\n" op1; token lexbuf}
					
  | "*." as op2  { printf "Operator 2 %s\n" op2; token lexbuf}				
  | '%'	 as op3	{ printf "Operator 3 %c\n" op3; token lexbuf}	
 
 (* arithmetic_operator *)
  | "!=" as op4 { printf "Operator 4 %s\n" op4; token lexbuf}				
  | '<'	 as op5 { printf "Operator 5 %c\n" op5; token lexbuf}			
  | "<=" as op6 { printf "Operator 6 %s\n" op6; token lexbuf} 
  | '>'	 as op7 { printf "Operator 7 %c\n" op7; token lexbuf}	
  | ">=" 
  | "=="				
 
 (* matrix_operator *)
  | ".*" as op8 { printf "Operator 8 %s\n" op8; token lexbuf}	
  | '''				
  | '"'  
 (* assignment_operator *)
 	| '=' as op9 { printf "Operator 9 %c\n" op9; token lexbuf}				
 	| "+="				
 	| "-="				
 	
 (* boolean_operator *)
 	| "&&"				
 	| "||"	as op10 { printf "Operator 10 %s\n" op10; token lexbuf}	
 	| '!'   as op11 { printf "Operator 11 %c\n" op11; token lexbuf}
 	
 (* punctuation *)
  | '('				
  | ')'					
  | '['					
  | ']'					
  | '{'					
  | '}'					
  | ';'					
  | ':'					
  | ','	as op12 { printf "Operator 12 %c\n" op12; token lexbuf}	
 	
 (* build-in_type *)
 	| "int"				
 	| "float"		
 	| "char"			
 	| "string"			
 	| "array"			
 	| "mat"				
 	| "vector"		
 	
 (* control_flow *)
	| "if"               
	| "else"             
	| "while"            
	| "for"              
	| "in"				
	| "endif"
  
 (* selection *)
  | "switch"     
 		
 (* boolean_true_false *) 
  | "true"            
  | "false" as op13 { printf "Operator 13 %s\n" op13; token lexbuf}
 	
  | integer as lit  { printf "Integer %s: %d\n" lit (int_of_string lit); token lexbuf }
  | (float1 | float2 | float3) as fl { printf "floag %s %f \n" fl (float_of_string fl); token lexbuf }
  | identifier as id { printf "ID %s\n" id; token lexbuf; }
  | eof                { raise Eof }
  
  | "/*"				{ comment lexbuf	}
  
and comment = parse
 	 "*/" { token lexbuf }
	| _     { comment lexbuf }

{
let main () = 
	let lexbuf = Lexing.from_channel stdin in 
	token lexbuf
	
	let _ = Printexc.print main ()
}
