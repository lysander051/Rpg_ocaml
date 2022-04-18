Random.self_init();;

module type OBJET_SIG =
sig
	type type_obj = | Poulet | Eponge | Piece | Rien
	val init_objet : unit -> type_obj
	val affiche_objet : type_obj -> int -> string
	val visuel_objet : type_obj -> int -> string
end;;

module Objet : OBJET_SIG =
struct 
	type type_obj = | Poulet | Eponge | Piece | Rien
	
	let init_objet : unit -> type_obj = fun () -> 
		let n= Random.int 4 in 
			match n with 
				| 0 -> Poulet
				| 1 -> Eponge
				| 2 -> Piece
				| _ -> Rien

	let affiche_objet : type_obj -> int -> string = fun obj n->
	match obj with 
			| Poulet when n=1 -> "poulet"
			| Poulet when n>1 -> "poulets"
			| Eponge when n=1 -> "éponge"
			| Eponge when n>1 -> "éponges"
			| Piece when n=1-> "pièce"
			| Piece when n>1-> "pièces"
			| Rien -> "rien"	
			| _ -> "rien"

	let visuel_objet : type_obj -> int -> string = fun obj n->
		match obj with 
			| Poulet when n=1 -> " Poulet  :  " ^ string_of_int(n)
			| Poulet when n>1 -> " Poulets :  " ^ string_of_int(n)
			| Eponge when n=1 -> " Eponge  :  " ^ string_of_int(n) 
			| Eponge when n>1 -> " Eponges :  " ^ string_of_int(n)
			| Piece when n=1->   " Pièce   :  " ^ string_of_int(n) 
			| Piece when n>1-> 	 " Pièces  :  " ^ string_of_int(n)
			| _ -> "                "

end;;