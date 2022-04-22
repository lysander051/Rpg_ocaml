open Objet;;

module type MONSTRE_SIG =
sig
  type type_monstre = Golem | Sanglier | Nuee of int
  type monstre = { creature : type_monstre; loot : Objet.type_obj; pv : int}
  val init_monstre : unit -> monstre
  val nom_monstre : monstre -> string
  val xp_gagne : monstre -> int
  val monstre_vaincu : monstre -> string
  val monstre_frapper : monstre -> float
  val message_combat : monstre -> float -> string
  val message_malheureuse_rencontre : monstre -> string
  val nom_monstre_tueur_nuit : monstre -> string
end;;

module Monstre : MONSTRE_SIG =
struct 

  (**
  Le type du monstre peut être un golem, un sanglier ou une nuée de moustiques
  @auteur Ravelonarivo Finaritra
  *)
  type type_monstre = Golem | Sanglier | Nuee of int


  (**
  Le monstre est une créature de type "type_monstre" possédant un objet de type "type_objet" 
  et ayant un point de vie de type "int"
  @auteur Ravelonarivo Finaritra
  *)
  type monstre = { creature : type_monstre ; loot : Objet.type_obj ; pv : int}


  (**
  Renvoie la somme de x nombres tirés aléatoirement entre 1 et n
  @auteur Ravelonarivo Finaritra
  @param x le nombre de chiffres à tirer
  @param n le chiffre maximum pouvant être tiré
  @return la somme de x nombres tirés aléatoirement entre 1 et n
  *)
  let rec d  : int -> int -> int = fun x n -> 
    match x with 
      | 0 -> 0
      | _ -> ((Random.int n)+1) +  d (x-1)  n


  (**
  Initialisation d'un monstre aléatoire possédant un objet aléatoire ou rien 
  et ayant un point de vie selon le nombre de moustique si c'est une nuée de moustiques 
  ou bien selon un critère en utilisant la fonction "d x n"
  @auteur Ravelonarivo Finaritra
  @return un monstre
  *)
  let init_monstre : unit -> monstre = fun () ->
    let x = Random.int 3 in 
    match x with 
      | 0 -> {creature = Golem ; loot = Objet.init_objet () ; pv = 25 +( d 1 6 ) }
      | 1 -> let moustique = 1+(Random.int 20) in 
             {creature = Nuee moustique ; loot = Rien ; pv = 2 + moustique }
      | _ -> {creature = Sanglier ; loot = Objet.init_objet (); pv = 10 +( d 1 4)  }    


  (**
  Le point de vie que le personnage perd quand le monstre frappe 
  @auteur Ravelonarivo Finaritra
  @param monstre le monstre qui frappe
  @return le dégat causé à un personnage quand le monstre frappe
  *)
  let monstre_frapper : monstre -> float = fun monstre ->
    let chance = Random.int 100 in 
    match monstre.creature with
        | Golem when chance < 30 -> 4.
        | Sanglier when chance < 50 -> 2. 
        | Nuee moustique when chance < 70 ->0.5 *. float(moustique)  
        | _ -> 0.
  

  (**
   Le point d'expérience gagné quand le personnage a vaincu le monstre 
   @auteur Ravelonarivo Finaritra
   @param monstre le monstre qui est vaincu
   @return le point d'expérience récupéré par le personnage quand il a gagné un combat
  *)
  let xp_gagne : monstre -> int = fun m-> 
    match m.creature with
      | Golem -> 8
      | Sanglier -> 4
      | Nuee _ -> 2


  (**
    La créature du monstre
    @auteur Ravelonarivo Finaritra
    @param m le monstre dont on veut savoir le nom
    @return le nom de la créature du monstre
  *)
  let nom_monstre : monstre -> string = fun m ->
    match m.creature with
      | Golem -> "Le golem"
      | Sanglier -> "Le sanglier"
      | Nuee _ -> "La nuée de moustiques"


  (**
    Un message personnalisé selon le monstre quand il attaque 
    ou bien un message quand il rate le personnage
    @auteur Ravelonarivo Finaritra
    @param m le monstre qui attaque
    @param degat le point de vie que le personnage perd si le monstre a réussi à frapper , 0 sinon
    @return un message de la situation du combat quand c'est le monstre qui frappe
  *)
  let message_combat : monstre -> float -> string = fun m degat ->
    if degat= 0. then
      "L'ennemi attaque mais vous manque.\n"
    else
      nom_monstre m ^ " vous attaque et vous perdez "^ (string_of_int (int_of_float degat)) ^ 
      if degat<=1. then
        " point de vie.\n"
      else
        " points de vie.\n"
  

  (**
    La créature du monstre qui a tué le personnage en dormant
    @auteur Ravelonarivo Finaritra
    @param m le monstre qui a tué le personnage quand ce dernier s'est endormi
    @return le nom de la créature  du monstre
  *)
  let nom_monstre_tueur_nuit : monstre -> string = fun m ->
    match m.creature with
      | Golem -> "Un golem"
      | Sanglier -> "Un sanglier"
      | Nuee _ -> "Une nuée de moustiques"

  (**
    Un message quand le monstre a été vaincu par le personnage
    @auteur Ravelonarivo Finaritra
    @param m le monstre qui est mort
    @return un message quand le monstre est mort
  *)
  let monstre_vaincu : monstre -> string = fun m ->
    let obj_recuperer = match m.loot with
      | Rien -> "mais dommage il ne vous a rien laissé"
      | Poulet -> "et vous récupérez un bon poulet"
      | Eponge ->  "et vous récupérez une éponge"
      | Piece -> "et vous vous enrichissez de plus en plus avec une pièce"
    in  
    "Vous avez survécu à l'attaque du monstre " ^ obj_recuperer ^ ".\n"^
    "Vous gagnez "^ (string_of_int (xp_gagne m ))^" points d'expérience.\n"


  (**
    Un message quand un monstre apparaît lors d'une malheureuse rencontre
    @auteur Badet Maxime
    @param monstre le monstre qui apparaît
    @return un message quand un monstre apparaît
  *)
  let message_malheureuse_rencontre : monstre -> string = fun monstre -> 
    match monstre.creature with
      | Golem ->"> Le sol tremble sous vos pied, vous commencez à vous destabiliser. \nquand soudain un golem apparait devant vous.\n"
      | Sanglier ->"> Une odeur forte que vous connaissez bien, vous parvient. \nUn sanglier sort des bois et vous attaque.\n"
      | Nuee a ->  "> Vous entendez un bourdonnement tout autour de vous. \nQuand soudain une nuée " ^
                  ( if a=1 then 
                      "d'un moustique " 
                    else 
                      "de " ^ string_of_int a ^ " moustiques " )
                   ^ " se jette sur vous.\n"
     

end;;




