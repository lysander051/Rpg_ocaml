module PERSONNAGE = 
struct
type classe = Archer | Guerrier | Magicien
  type genre = Homme | Femme
  type objet = Poulet | Eponge | Piece
  type qte_objet = { obj : objet; quantite : int }
      
  type sac = { poulet : int; eponge : int; piece : int } 
  type perso = { nom : string; sexe : genre; role : classe; pv : int; xp : int; le_sac : sac; niveau : int } 
  
  let classe_genre = fun perso -> match (perso.sexe, perso.role) with
    | (Homme, Archer) -> "Archer"
    | (Homme, Guerrier) -> "Guerrier"
    | (Homme, Magicien) -> "Magicien" 
    | (Femme, Archer) -> "Archere"
    | (Femme, Guerrier) -> "Guerriere"
    | (Femme, Magicien) -> "Magicienne"

  let contenu_sac = fun sac ->  string_of_int sac.poulet ^ " poulet" ^ "\n" ^ 
                                string_of_int sac.eponge ^ " éponges" ^ "\n" ^
                                string_of_int sac.piece ^ " pièces"
  let afficher_sac = fun sac -> print_string(contenu_sac sac)
