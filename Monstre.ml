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

  
   
  let init_monstre = 
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
    in s ^ "  obj =  " ^ (Objet.affiche_objet monstre.loot) ^ "  pv =  " ^ (string_of_int monstre.pv) ^"\n"

    let monstre_frapper : monstre -> float =fun monstre ->
     match monstre.creature with
      | Golem -> 4.
      | Sanglier -> 2. 
      | Nuee moustique ->0.5 *. float(moustique)  
end;;



(*Random.self_init();;
let lemonstre= Monstre.init_monstre;;
print_string (Monstre.affiche_monstre lemonstre);;*)
