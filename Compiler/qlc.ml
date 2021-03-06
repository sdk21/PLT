type action = Ast | Sast | Gen | Debug

let _ =
  let action =
    List.assoc Sys.argv.(1) [("-a", Ast); ("-s", Sast); ("-g", Gen); ("-d", Debug);]
  in
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(2)) (*stdin *) and
    output_file = String.sub Sys.argv.(2) 0 (String.length(Sys.argv.(2))-3) in
      let program = Parser.program Scanner.token  lexbuf in
        match action with 
          Ast ->  print_string (Ast.string_of_program program)
          | Sast -> 
            let sprogram =
              Analyzer.check_program program
            in
              print_string (Sast.string_of_sprogram sprogram)
          | Gen -> Generator.gen_program output_file (Analyzer.check_program program)
          | Debug -> print_string "debug"
