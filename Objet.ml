Random.self_init();;
module Objet =
struct 
	type type_obj= | Poulet | Eponge | Piece | Rien
	
	let affiche_objet = fun obj n->
	match obj with 
			| Poulet when n=1 -> "poulet"
			| Poulet when n>1 -> "poulets"
			| Eponge when n=1 -> "eponge"
			| Eponge when n>1 -> "eponges"
			| Piece when n=1-> "piece"
			| Piece when n>1-> "pieces"
			| Rien -> "rien"	
			| _ -> "rien"

	let visuel_objet = fun obj n->
		match obj with 
			| Poulet when n=1 -> " Poulet  :  " ^ string_of_int(n)
			| Poulet when n>1 -> " Poulets :  " ^ string_of_int(n)
			| Eponge when n=1 -> " Eponge  :  " ^ string_of_int(n)
			| Eponge when n>1 -> " Eponges :  " ^ string_of_int(n)
			| Piece when n=1->   " Pièce   :  " ^ string_of_int(n)
			| Piece when n>1-> 	 " Pièces  :  " ^ string_of_int(n)
			| _ -> "                     "

	let init_objet = 
		let n= Random.int 4 in 
			match n with 
				| 0 -> Poulet
				| 1 -> Eponge
				| 2 -> Piece
				| _ -> Rien

end;;