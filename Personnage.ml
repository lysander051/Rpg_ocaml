open Objet;;

module Personnage = 
struct 
  type classe = Archer | Guerrier | Magicien
  type genre = Homme | Femme
  type objet={type_obj : Objet.type_obj ; qte : int}
  type sac= objet list
  type perso = { nom : string ; sexe : genre ; role : classe ; pv : float ; xp :int  ; niveau : int  ; sac : sac}
  
  let classe_genre = fun perso -> match (perso.sexe, perso.role) with
    | (Homme, Archer) -> "Archer"
    | (Homme, Guerrier) -> "Guerrier"
    | (Homme, Magicien) -> "Magicien" 
    | (Femme, Archer) -> "Archere"
    | (Femme, Guerrier) -> "Guerriere"
    | (Femme, Magicien) -> "Magicienne"
  
  exception Champs_Vide
  exception Personnage_mort

  let init_perso = fun n -> fun g -> fun r ->
    {nom = n; sexe = g; role = r; pv = 10.; xp = 0; niveau = 1; sac = [{type_obj = Objet.Eponge; qte = 2};{type_obj = Objet.Poulet; qte = 1};{type_obj = Objet.Piece; qte = 2}] }

      (*let remplir_sac = fun x y z -> { poulet = x; eponge = y; piece = z }  *)
  let etat_perso = fun perso -> perso.nom ^ " | " ^ (classe_genre perso) ^ "  Niveau " ^ string_of_int (perso.niveau) ^ "\n"
                                ^ "Points de vie  | " ^ string_of_float perso.pv ^ "\n" ^
                                "Expérience  | " ^ string_of_int perso.xp 
  let afficher_infos_perso = fun perso -> print_string(etat_perso perso)

let mis_a_jour_pv = fun ajoutPv-> fun perso ->
  if perso.pv +. ajoutPv > 20. then {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = 20.; xp = perso.xp; niveau = perso.niveau; sac = perso.sac }
  else if (perso.pv +. ajoutPv > 20. && perso.pv +. ajoutPv <0.) then {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = perso.pv-.ajoutPv; xp = perso.xp; niveau = perso.niveau; sac = perso.sac }
  else raise Personnage_mort



let frapper : perso -> int = fun perso ->
  let chance = Random.int 100 in  let add_bonus=5*((perso.niveau) -1 ) in
  match perso.role with 
  | Archer when chance <70 + add_bonus -> 4
  | Magicien when chance <50 +add_bonus ->5
  | Guerrier when chance < 30 + add_bonus ->10
  | _ -> 0

  let avoir_un_poulet: perso -> bool = fun pers ->
    let rec aux = fun sac -> match sac with
  | [] -> false
  | {type_obj=a; qte=b}::_ when a=Poulet && b>0 -> true
  | h::t -> false || aux t
  in aux pers.sac

  let retirer_objet = fun obj n perso ->
    let rec aux = fun obj n nouveauSac persoSac ->
      match persoSac with
      | [] -> {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = perso.pv; xp = perso.xp; niveau = perso.niveau; sac = nouveauSac }
      | {type_obj=a; qte=b}::t when a=obj && b=n -> aux obj n (nouveauSac) t
      | {type_obj=a; qte=b}::t when a=obj && b>n -> aux obj n ({type_obj=a; qte=b-n}::nouveauSac) t 
      | h::t -> aux obj n (h::nouveauSac) t
    in aux obj n [] perso.sac

  

let manger : perso -> (bool *perso) = fun perso ->
  if perso.pv>=20. || (not(avoir_un_poulet perso) )  then (false,perso)
  else 
    let perso = mis_a_jour_pv 2. perso in
    let perso = retirer_objet Objet.Poulet 1 perso in
    (true,perso)

let rec changement_niveau :int -> int -> int*int = fun niv xp  ->
  let niv_1 = (2**niv )*10 in let niv_2= (2**(niv+1))*10 in 
  if xp >= niv_2 then changement_niveau niv+1 xp
  else let nouv_xp = xp - niv_1 in (niv,nouv_xp)

end;;

