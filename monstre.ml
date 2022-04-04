open Objet
module MONSTRE =
struct 

  type type_monstre = Golem | Sanglier | Nuee of int
  type monstre = { creature : type_monstre; loot : OBJET.objet; pv : int}

  let rec (d) = fun x n -> 
    match x with 
      | 0 -> 0
      | _ -> (Random.int n) +  d (x-1)  n

  

  let generer_obj_monstre = 
    let n= Random.int 3 in 
    let type_o = 
        match n with 
          | 0 -> OBJET.Poulet
          | 1 -> OBJET.Eponge
          | 2 -> OBJET.Piece
    in let nb = Random.int 5 in
        if nb>0 then  {OBJET.type_obj = type_o ; OBJET.qte =nb}
        else {type_obj= Rien;qte=nb}
   
  let generer_monstre = 
    let x = Random.int 3 in 
    match x with 
      | 0 -> {creature = Golem ; loot = generer_obj_monstre ; pv = 25 +( d 1 6 ) }
      | 1 -> let moustique = (Random.int 25) in 
              let objet= {OBJET.type_obj=Rien ; OBJET.qte = 0} in
             {creature = Nuee moustique ; loot = objet ; pv = 2 + moustique }
      | 2 -> {creature = Sanglier ; loot = generer_obj_monstre ; pv = 10 +(  d 1 4)  }

  
    let affiche_monstre=  fun monstre -> let s = match monstre.creature with
      | Golem -> "golem"
      | Nuee a -> " nuee de moustique"
      | Sanglier ->"sanglier"
    in s ^ "obj = " ^ (OBJET.affiche_objet monstre.loot) ^ "pv = " ^ (string_of_int monstre.pv)
end;;




(*let lemonstre= MONSTRE.generer_monstre;*)
(*print_string (MONSTRE.affiche_monstre lemonstre);;*)