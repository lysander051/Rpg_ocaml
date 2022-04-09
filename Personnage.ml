open Objet;;

module Personnage = 
struct 
  type classe = Archer | Guerrier | Magicien
  type genre = Homme | Femme
  type objet={type_obj : Objet.type_obj ; qte : int}
  type sac= objet list
  type perso = { nom : string ; sexe : genre ; role : classe ; pv : int ; xp :int  ; niveau : int  ; sac : sac}
  
  let classe_genre = fun perso -> match (perso.sexe, perso.role) with
    | (Homme, Archer) -> "Archer"
    | (Homme, Guerrier) -> "Guerrier"
    | (Homme, Magicien) -> "Magicien" 
    | (Femme, Archer) -> "Archere"
    | (Femme, Guerrier) -> "Guerriere"
    | (Femme, Magicien) -> "Magicienne"
  
  exception Champs_Vide

  let init_perso = fun n -> fun g -> fun r ->
    let gen = if (g="H") then Homme else Femme in
    let rol = if (r="A") then Archer else if (r="G") then Guerrier else Magicien in
    {nom = n; sexe = gen; role = rol; pv = 10; xp = 0; niveau = 1}

      (*let remplir_sac = fun x y z -> { poulet = x; eponge = y; piece = z }  *)
  let etat_perso = fun perso -> perso.nom ^ " | " ^ (classe_genre perso) ^ "  Niveau " ^ string_of_int (perso.niveau) ^ "\n"
                                ^ "Points de vie  | " ^ string_of_int perso.pv ^ "\n" ^
                                "Expérience  | " ^ string_of_int perso.xp 
  let afficher_infos_perso = fun perso -> print_string(etat_perso perso)

  let mis_a_jour_pv :int -> int = fun nouv_pv -> if nouv_pv > 20 then 20 else nouv_pv 
let chance_de_toucher : int -> int = fun x -> Random.int x

let frapper : perso -> int = fun perso ->
  let chance = chance_de_toucher 100 in  let add_bonus=5*((perso.niveau) -1 ) in
  match perso.role with 
  | Archer when chance <70 + add_bonus -> 4
  | Magicien when chance <50 +add_bonus ->5
  | Guerrier when chance < 30 + add_bonus ->10
  | _ -> 0

  let  avoir_un_poulet: perso -> bool = fun pers ->
    let rec aux = fun sac -> match sac with
  | [] -> false
  | {type_obj=a; qte=b}::_ when a=Poulet && b>0 -> true
  | h::t -> false || aux t
  in aux pers.sac

  let  rec retirer_un_objet_dans_le_sac : Objet.type_obj -> int ->'a list -> 'a list = 
  fun obj n sac ->

    match sac with
    | [] -> []
    | {type_obj=a; qte=b}::t when a=obj && b=n -> retirer_un_objet_dans_le_sac obj n t
    | {type_obj=a; qte=b}::t when a=obj && b>n -> {type_obj=a; qte=b-n} ::retirer_un_objet_dans_le_sac obj n t
    | h::t -> h :: retirer_un_objet_dans_le_sac obj n t

let manger : perso -> (bool *perso) = fun perso ->
  if perso.pv>=20 || (not(avoir_un_poulet perso) )  then (false,perso)
  else 
    let nouv_pv= if(perso.pv+2>20) then 20 else perso.pv+2 in
    let nouv_sac= retirer_un_objet_dans_le_sac Poulet 1 perso.sac in
  let nouv_pers={ nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = nouv_pv; xp = perso.xp; niveau = perso.niveau ; sac = nouv_sac } 
  in (true,nouv_pers)

end;;

print_string("\n");;


