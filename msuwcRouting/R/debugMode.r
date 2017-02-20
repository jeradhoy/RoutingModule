debugInFunction <- function(){
	print("Type commands in evaluate.")
	print("Type 'exit' to continue.")
	stp <- F
	while(!stp){
		input <- readline()
		if(input == "exit"){
			stp <- T
		} else {
			try(print(eval(parse(text=input))))
		}
	}
}
