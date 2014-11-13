let rock target_file = 
	Sys.command ("javac " ^ target_file ^ ".java Matrix.java && java " ^ target_file )   
