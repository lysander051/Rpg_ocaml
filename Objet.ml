Random.self_init();;
module Objet =
struct 
	type type_obj= | Poulet | Eponge | Piece | Rien
	
	let affiche_objet = fun obj ->
	match obj with 
			| Poulet -> "poulet"
			| Eponge -> "eponge"
			| Piece -> "piece"
			| Rien -> "rien"	

	let init_objet = 
		let n= Random.int 4 in 
			match n with 
				| 0 -> Poulet
				| 1 -> Eponge
				| 2 -> Piece
				| _ -> Rien

end;;