open Objet;;
open Monstre;;
open Personnage;;


exception Quitte_le_jeu;;

let delimiteur = fun () ->"+--------------------------------------------------------------------------------+\n"

let rec read_nom = fun () ->
  let () = print_string (delimiteur() ^ ">Ton nom: ") in
  let n = read_line() in
  if n="" then (print_string "nom incorrecte\n"; read_nom())
  else n
;;

let rec read_genre = fun () ->
  let () = print_string (delimiteur() ^ 
">Ton genre: 
  F) Femme
  H) Homme
Votre choix: ") in
let g=read_line() in
if not(g="F" || g="H") then (print_string "tu DOIS avoir un genre\n"; read_genre())
else (if g="F" then Personnage.Femme else Personnage.Homme)
;;

let rec read_classe = fun g ->
  let () = 
  if g = Personnage.Homme then 
    print_string (delimiteur() ^ 
">Ta classe: 
  A) Archer
  G) Guerrier
  M) Magicien
Votre choix: ") 
  else
    print_string (delimiteur() ^ 
">Ta classe: 
  A) Archere
  G) Guerriere
  M) Magicienne
Votre choix: ")
  in 
  let c =read_line() in
  if not(c="A" || c="G" || c="M") then (print_string "il faut choisir une classe\n"; read_classe(g))
  else (if c="A" then Personnage.Archer else if c="G" then Personnage.Guerrier else Personnage.Magicien)
;;

let rec read_action = fun() ->
  let () = print_string (delimiteur() ^ 
">Que voulez-vous faire
  A) Attaquer  
  F) Fuir 
  V) Voir l'état de votre perso
Votre choix: ") in
  let c = read_line() in
  if not(c="A" || c="F" || c="V") then (print_string "il faut faire un choix\n"; read_action())
  else c
;;

let rec read_hubAventure = fun () ->
  let () = print_string (delimiteur() ^
">Que voulez-vous faire?
 C) Continuer votre chemin
 D) Dormir
 M) Manger
 V) Visualiser l'état du personnage
 Q) Quitter l'aventure
Votre choix:") in
let c = read_line() in
  if not(c="C" || c="D" || c="M" || c="V" || c="Q") then (print_string "il faut faire un choix\n"; read_hubAventure())
  else c
;;

let rec init_aventure = fun ()->
  let n = read_nom() in
  let g = read_genre() in
  let c = read_classe g in
  Personnage.init_perso n g c
;;

let fuir : Personnage.perso -> Personnage.perso = fun perso ->
  let taille = List.length(perso.sac) in
  let obj = List.nth perso.sac (Random.int taille) in
  let () = print_string (delimiteur() ^">Vous perdez 1 " ^ Objet.affiche_objet obj.type_obj 1 ^"\n") in
  Personnage.retirer_objet obj.type_obj 1 perso
;;

let combattre : Personnage.perso -> Monstre.monstre -> Personnage.perso = fun pers monstre ->
  let rec le_combat :int -> Personnage.perso -> Monstre.monstre -> Personnage.perso = fun attaquant p m -> 
    match attaquant  with 
      |0 -> let pv_monstre= m.pv - Personnage.frapper p in 
        if (pv_monstre <=0 ) then 
          let nouv_xp= p.xp +( Monstre.xp_gagne monstre) in Monstre.monstre_vaincu m ;
          Personnage.changement_niveau p nouv_xp
        else let nouv_monstre : Monstre.monstre = {creature = monstre.creature; loot = monstre.loot; pv = pv_monstre}
        in le_combat 1 p nouv_monstre
      |_-> let degat = (Monstre.monstre_frapper m) in (print_string (Monstre.message_combat m degat)) ; let nouv_pers=(Personnage.mis_a_jour_pv (-. degat ) p) 
        in le_combat 0 nouv_pers m
     in le_combat (Random.int 2) pers monstre
;; 

let malheureuse_rencontre = fun perso->
  let monstre = Monstre.init_monstre in
  let () = 
  if monstre.creature = Monstre.Golem then print_string (delimiteur() ^ ">Le sol tremble sous vos pied, vous êtes destabilisé quand soudain un golem apparait devant vous.\n")
  else if monstre.creature = Monstre.Sanglier then print_string (delimiteur() ^ ">Une odeur forte que vous connaissez bien, vous parvient. Un sanglier sort des bois et vous attaque.\n")
  else print_string (delimiteur() ^ ">Vous entendez un bourdonnement tout autour de vous. quand soudain une nué de moustique se jette sur vous.\n")
  in   
  let rec aux = fun perso ->
    let choix = read_action() in
    if choix = "A" then (combattre perso monstre)
    else if choix = "F" then fuir perso
    else (print_string (delimiteur()); Personnage.afficher_infos_perso perso; aux perso)
  in
  aux perso
;;

let rec hubAventure = fun perso ->
  let c = read_hubAventure() in
  if      c="C" then hubAventure (malheureuse_rencontre perso)
  else if c="D" then (print_string (delimiteur() ^ ">Vous installez votre campement et tombez rapidement endormie.\n"); hubAventure (Personnage.dormir perso))
  else if c="M" then 
    (let mange = Personnage.manger perso in 
    if (fst mange) then (print_string (delimiteur() ^ ">Vous mangez un peu avant de reprendre votre aventure.\n"); hubAventure (snd mange))
    else (print_string (delimiteur() ^ ">Vous n'avez pas à manger\n"); hubAventure (snd mange)))
  else if c="V" then (print_string (delimiteur()); Personnage.afficher_infos_perso perso; hubAventure perso)
  else raise Quitte_le_jeu
;;