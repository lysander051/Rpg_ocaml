module OBJET =
struct 
	type type_obj= | Poulet | Eponge | Piece | Rien
	type quantite=int
	type objet={type_obj : type_obj ; qte : quantite}

	let affiche_objet = fun obj ->
		let s= match obj.type_obj with 
			| Poulet -> "poulet"
			| Eponge -> "eponge"
			| Piece -> "piece"
			| Rien -> "rien"
		in
		s ^ if(s !="rien") then (string_of_int obj.qte) else ""

	end
