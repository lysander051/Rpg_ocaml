open Objet;;

module Personnage = 
struct 
  type classe = Archer | Guerrier | Magicien
  type genre = Homme | Femme
  type perso = { nom : string; sexe : genre; role : classe; pv : int; xp : int; niveau : int } 
  
  let classe_genre = fun perso -> match (perso.sexe, perso.role) with
    | (Homme, Archer) -> "Archer"
    | (Homme, Guerrier) -> "Guerrier"
    | (Homme, Magicien) -> "Magicien" 
    | (Femme, Archer) -> "Archere"
    | (Femme, Guerrier) -> "Guerriere"
    | (Femme, Magicien) -> "Magicienne"
  
  
  type sac = { objet : objet list ; quantite : int}
  let init_perso = fun nom sexe role -> { nom = nom; sexe = sexe; role = role; pv = 20; xp = 0 ; niveau = 1} 
  

      (*let remplir_sac = fun x y z -> { poulet = x; eponge = y; piece = z }  *)
  let etat_perso = fun perso -> perso.nom ^ " | " ^ (classe_genre perso) ^ "  Niveau " ^ string_of_int (perso.niveau) ^ "\n"
                                ^ "Points de vie  | " ^ string_of_int perso.pv ^ "\n" ^
                                "Expérience  | " ^ string_of_int perso.xp 
  let afficher_infos_perso = fun perso -> print_string(etat_perso perso)
end;;

print_string("\n");;


