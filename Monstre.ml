open Objet;;
(*Random.self_init();;*)
module Monstre =
struct 

  type type_monstre = Golem | Sanglier | Nuee of int
  type monstre = { creature : type_monstre; loot : Objet.type_obj; pv : int}

  let rec d  : int -> int -> int = fun x n -> 
    match x with 
      | 0 -> 0
      | _ -> (Random.int n) +  d (x-1)  n

  let init_monstre : unit -> monstre = fun () ->
    let x = Random.int 3 in 
    match x with 
      | 0 -> {creature = Golem ; loot = Objet.init_objet() ; pv = 25 +( d 1 6 ) }
      | 1 -> let moustique = 1+(Random.int 20) in 
             {creature = Nuee moustique ; loot = Rien ; pv = 2 + moustique }
      | _ -> {creature = Sanglier ; loot = Objet.init_objet (); pv = 10 +(  d 1 4)  }    
  
  let affiche_monstre : monstre -> unit= fun monstre -> let s = match monstre.creature with
    | Golem -> "Le golem"
    | Nuee a -> "La nuée composée de " ^ string_of_int a ^ "moustiques"
    | Sanglier ->"Le sanglier"
  in
  let point_vie = match  monstre.pv with 
| 1 -> " a "^ (string_of_int monstre.pv) ^" point de vie "
| _ -> " a "^ (string_of_int monstre.pv) ^ "points de vie "
in
  let a_objet =match monstre.loot with
    |Rien ->" et ne possède pas d'objet à récupérer"
    |Eponge -> " et possède une éponge pouvant être récupérer"
    |Piece -> " et possède une pièce "
    |Poulet -> " et possède un poulet à récupérer"
in  print_string(s ^point_vie ^ a_objet ^ "\n" )

  let monstre_frapper : monstre -> float = fun monstre ->
    let chance = Random.int 100 in 
    match monstre.creature with
        | Golem when chance < 30 -> 4.
        | Sanglier when chance < 50 -> 2. 
        | Nuee moustique when chance < 70 ->0.5 *. float(moustique)  
        | _ -> 0.

  let xp_gagne : monstre -> int = fun m-> match m.creature with
    | Golem -> 8
    | Sanglier -> 4
    | Nuee _ -> 2

  let message_combat : monstre -> float -> string = fun m degat ->
    if degat= 0. then
      "L'ennemi attaque mais vous manque\n"
    else
      match m.creature with 
        | Golem -> "Le golem vous attaque et vous perdez "^ (string_of_int (int_of_float degat)) ^ " points de vie\n"
        | Sanglier -> "Le sanglier vous attaque et vous perdez "^ (string_of_int (int_of_float degat)) ^ " points de vie\n"
        | Nuee _ -> "La nuée de moustiques vous attaque et vous perdez "^ (string_of_float degat) ^ " points de vie\n"
        
  let nom_monstre = fun m ->
    match m.creature with
    | Golem -> "Le golem"
    | Sanglier -> "Le sanglier"
    | Nuee _ -> "La nuée de moustique"

    let nom_monstre_tueur_nuit = fun m ->
      match m.creature with
      | Golem -> "Un golem"
      | Sanglier -> "Un sanglier"
      | Nuee _ -> "Une nuée de moustique"

  let monstre_vaincu : monstre -> unit = fun m ->let obj_recuperer = 
    match m.loot with
    | Rien -> "mais dommage il ne vous a rien laissé "
    | Poulet -> "et vous récupérer un bon poulet"
    | Eponge ->  "et vous récupérer une éponge"
    | Piece -> "et vous vous enrichissez de plus en plus avec une pièce"
  in  
    print_string ("Vous avez survécu à l'attaque du monstre " ^ obj_recuperer ^ "\n")

  let message_malheureuse_rencontre : monstre -> string = fun monstre -> 
    match monstre.creature with
      | Golem ->"> Le sol tremble sous vos pied, vous êtes destabilisé. \nquand soudain un golem apparait devant vous.\n"
      | Sanglier ->"> Une odeur forte que vous connaissez bien, vous parvient. \nUn sanglier sort des bois et vous attaque.\n"
      | Nuee a ->  "> Vous entendez un bourdonnement tout autour de vous. \nQuand soudain une nuée " ^
      (if a=1 then "d'un moustique " else "de " ^ string_of_int a ^ " moustiques " )
      ^ " se jette sur vous.\n"
     
end;;



(*Random.self_init();;
let lemonstre= Monstre.init_monstre;;
print_string (Monstre.affiche_monstre lemonstre);;*)
