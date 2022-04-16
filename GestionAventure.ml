open Objet;;
open Monstre;;
open Personnage;;


exception Quitte_le_jeu;;

let delimiteur = fun () ->"\n+--------------------------------------------------------------------------------+\n"

let rec read_nom  : unit -> string= fun () ->
  let () = print_string (delimiteur() ^ ">Ton nom: ") in
  let n = read_line() in
  if n="" then 
    (print_string "Il faut choisir un nom de personnage.\n"; read_nom())
  else if (String.length n) > 40 then
    (print_string "Le nom est trop long.\n"; read_nom())
  else 
    n
;;

let rec read_genre : unit -> Personnage.genre= fun () ->
  let () = print_string (delimiteur() ^ 
"> Ton genre: 
  F) Femme
  H) Homme
Votre choix: ") in
let g=read_line() in
if 
  not(g="F" || g="H" || g="f" || g="h") then (print_string "tu DOIS avoir un genre\n"; read_genre())
else 
  (if g="F" || g="f" then Personnage.Femme else Personnage.Homme)
;;

let rec read_classe : Personnage.genre -> Personnage.classe  = fun g ->
  let () = 
  if g = Personnage.Homme then 
    print_string (delimiteur() ^ 
"> Ta classe: 
  A) Archer
  G) Guerrier
  M) Magicien
Votre choix: ") 
  else
    print_string (delimiteur() ^ 
"> Ta classe: 
  A) Archère
  G) Guerrière
  M) Magicienne
Votre choix: ")
  in 
  let c =read_line() in
  if not(c="A" || c="G" || c="M" || c="a" || c="g" || c="m") 
    then (print_string "il faut choisir une classe\n"; read_classe(g))
  else 
    (if c="A" || c="a" then 
      Personnage.Archer 
    else if c="G" || c="g"
      then Personnage.Guerrier 
    else 
      Personnage.Magicien)
;;

let rec read_action : unit -> string  = fun() ->
  let () = print_string (delimiteur() ^ 
"> Que voulez-vous faire
  A) Attaquer  
  F) Fuir 
  V) Voir l'état de votre perso
Votre choix: ") in
  let c = read_line() in
  if not(c="A" || c="F" || c="V" || c="a" || c="f" || c="v") 
    then (print_string "il faut faire un choix\n"; read_action())
  else 
    c
;;

let rec read_hubAventure : unit -> string= fun () ->
  let () = print_string (delimiteur() ^
"> Que voulez-vous faire?
 C) Continuer votre chemin
 D) Dormir
 M) Manger
 V) Visualiser l'état du personnage
 Q) Quitter l'aventure
Votre choix:") in
let c = read_line() in
  if not(c="C" || c="D" || c="M" || c="V" || c="Q" || c="c" || c="d" || c="m" || c="v" || c="q") 
    then (print_string "il faut faire un choix\n"; read_hubAventure())
  else 
    c
;;

let rec init_aventure : unit -> Personnage.perso= fun ()->
  let n = read_nom() in
  let g = read_genre() in
  let c = read_classe g in
  Personnage.init_perso n g c
;;

let combattre : Personnage.perso -> Monstre.monstre -> Personnage.perso = fun pers monstre ->
  let rec le_combat :int -> Personnage.perso -> Monstre.monstre -> Personnage.perso = fun attaquant p m -> 
    match attaquant  with 
      |0 -> let frappe=Personnage.frapper p in Personnage.affiche_attaque p frappe ; let pv_monstre= m.pv - frappe  in
        if (pv_monstre <=0 ) then 
          let nouv_xp= p.xp +( Monstre.xp_gagne monstre) in Monstre.monstre_vaincu m ;
          Personnage.modifier_sac monstre.loot 1 (Personnage.changement_niveau p nouv_xp) 
        else let nouv_monstre : Monstre.monstre = {creature = monstre.creature; loot = monstre.loot; pv = pv_monstre}
        in le_combat 1 p nouv_monstre
      |_-> let degat = (Monstre.monstre_frapper m) in (print_string (Monstre.message_combat m degat)) ; let nouv_pers=(Personnage.mis_a_jour_pv (-. degat ) p) 
        in le_combat 0 nouv_pers m
     in le_combat (Random.int 2) pers monstre
;; 

let malheureuse_rencontre : Personnage.perso -> Personnage.perso= fun perso->
  let rand = Random.int 100 in
  if rand < 50 then 
    let monstre = Monstre.init_monstre() in
    let () = 
    print_string (delimiteur() ^ Monstre.message_malheureuse_rencontre monstre)
   in
    (combattre perso monstre)
  else 
    perso
;;

(** EXTENSION
let fuir : Personnage.perso -> Personnage.perso = fun perso ->
  let taille = List.length(perso.sac) in
  if 0 < taille then 
    (let obj = List.nth perso.sac (Random.int taille) in
    let () = print_string (delimiteur() ^"> Vous perdez 1 " ^ Objet.affiche_objet obj.type_obj 1 ^"\n") in
    let personnage = Personnage.modifier_sac obj.type_obj (-1) perso in
    let rand = Random.int 10 in
    if rand < 1 then malheureuse_rencontre personnage
    else personnage)
  else perso
;;

  let continuerAventure = fun perso ->
  let monstre = Monstre.init_monstre() in
  let () = 
  if monstre.creature = Monstre.Golem then print_string (delimiteur() ^ "> Le sol tremble sous vos pied, vous êtes destabilisé. \nquand soudain un golem apparait devant vous.\n")
  else if monstre.creature = Monstre.Sanglier then print_string (delimiteur() ^ "> Une odeur forte que vous connaissez bien, vous parvient. \nUn sanglier sort des bois et vous attaque.\n")
  else print_string (delimiteur() ^ "> Vous entendez un bourdonnement tout autour de vous. \nQuand soudain une nué de moustique se jette sur vous.\n")
  in   
  let rec aux = fun perso ->
    let choix = read_action() in
    if choix = "A" || choix = "a" then (combattre 0 perso monstre)
    else if choix = "F" || choix = "f" then fuir perso
    else (print_string (delimiteur()); Personnage.afficher_infos_perso perso; aux perso)
  in
  aux perso
;;
*)

let rec hubAventure : Personnage.perso -> unit= fun perso ->
  let c = read_hubAventure() in
  if      c="C" || c="c" then (print_string (delimiteur() ^ "> Vous continuez votre chemin vers votre prochaine destination.\n"); hubAventure (malheureuse_rencontre perso))
  else if c="D" || c="d" then (print_string (delimiteur() ^ "> Vous installez votre campement et tombez rapidement endormie.\n"); hubAventure (malheureuse_rencontre (Personnage.dormir perso)))
  else if c="M" || c="m" then 
    (let mange = Personnage.manger perso in 
    if (fst mange) then (print_string (delimiteur() ^ "> Vous mangez un peu avant de reprendre votre aventure.\n"); hubAventure (malheureuse_rencontre (snd mange)))
    else (print_string (delimiteur() ^ "> Vous n'avez pas à manger\n"); hubAventure (malheureuse_rencontre (snd mange))))
  else if c="V" || c="v" then (print_string (delimiteur()); Personnage.afficher_infos_perso perso; hubAventure perso)
  else raise Quitte_le_jeu
;;