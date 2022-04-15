open Objet;;
(*Random.self_init();;*)
module Monstre =
struct 

  type type_monstre = Golem | Sanglier | Nuee of int
  type monstre = { creature : type_monstre; loot : Objet.type_obj; pv : int}

  let rec (d) = fun x n -> 
    match x with 
      | 0 -> 0
      | _ -> (Random.int n) +  d (x-1)  n

  let init_monstre = fun () ->
    let x = Random.int 3 in 
    match x with 
      | 0 -> {creature = Golem ; loot = Objet.init_objet ; pv = 25 +( d 1 6 ) }
      | 1 -> let moustique = (Random.int 25) in 
             {creature = Nuee moustique ; loot = Rien ; pv = 2 + moustique }
      | _ -> {creature = Sanglier ; loot = Objet.init_objet ; pv = 10 +(  d 1 4)  }    
  
  let affiche_monstre=  fun monstre -> let s = match monstre.creature with
    | Golem -> "golem"
    | Nuee a -> " nuee de moustique"
    | Sanglier ->"sanglier"
  in s ^ "  obj =  " ^ (Objet.affiche_objet monstre.loot 1) ^ "  pv =  " ^ (string_of_int monstre.pv) ^"\n"

  let monstre_frapper : monstre -> float =fun monstre ->
    let chance = Random.int 100 in 
    if chance < 50 then 0.
    else
      match monstre.creature with
        | Golem -> 4.
        | Sanglier -> 2. 
        | Nuee moustique ->0.5 *. float(moustique)  

  let xp_gagne : monstre -> int = fun m-> match m.creature with
    | Golem -> 8
    | Sanglier -> 4
    | Nuee _ -> 2

  let message_combat : monstre -> float -> string = fun m degat ->
    if degat= 0. then
      "L'ennemi attaque mais vous manque\n"
    else
      match m.creature with 
        | Golem -> "Le golem vous attaque et vous perdez "^ (string_of_float degat) ^ " points de vie\n"
        | Sanglier -> "Le sanglier vous attaque et vous perdez "^ (string_of_float degat) ^ " points de vie\n"
        | Nuee _ -> "La nuée de moustique vous attaque et vous perdez "^ (string_of_float degat) ^ " points de vie\n"
        
  let nom_monstre = fun m ->
    match m.creature with
    | Golem -> "Le golem"
    | Sanglier -> "Le sanglier"
    | Nuee _ -> "La nuée de moustique"

  let monstre_vaincu : monstre -> unit = fun m -> print_string "Vous avez survécu à l'attaque du monstre \n"

end;;



(*Random.self_init();;
let lemonstre= Monstre.init_monstre;;
print_string (Monstre.affiche_monstre lemonstre);;*)
