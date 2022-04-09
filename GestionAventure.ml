open Objet;;
open Monstre;;
open Personnage;;

exception Tue_En_Dormant of Monstre.monstre ;;

let dormir : Personnage.perso -> Personnage.perso = 
    fun perso -> let chance_monstre = Random.int 100 in
    if (chance_monstre<5) then let lemonstre = Monstre.init_monstre in raise (Tue_En_Dormant lemonstre)
    else 
      let nouv_pv= Personnage.mis_a_jour_pv (perso.pv +4) in 
      { nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = nouv_pv; xp = perso.xp; niveau = perso.niveau ; sac = perso.sac };;

let rec read_nom = fun () ->
  let () = print_string "Ton nom: " in
  let n = read_line() in
  if n="" then (print_string "nom incorrecte\n\n\n"; read_nom())
  else n;;

let rec read_genre = fun () ->
  let () = print_string "
Ton genre: 
  F) Femme
  H) Homme
Votre choix: " in
let g=read_line() in
if not(g="F" || g="H") then (print_string "tu DOIS avoir un genre\n\n"; read_genre())
else g;;

let rec read_classe = fun g ->
  let () = 
  if g = "H" then 
    print_string "
Ta classe: 
  A) Archer
  G) Guerrier
  M) Magicien
Votre choix: " 
  else
    print_string "
Ta classe: 
  A) Archere
  G) Guerriere
  M) Magicienne
Votre choix: "
  in 
  let c =read_line()in
  if not(c="A" || c="G" || c="M") then (print_string "il faut choisir une classe\n\n"; read_classe(g))
  else c;;

let rec init_aventure = fun ()->
  let n = read_nom() in
  let g = read_genre() in
  let c = read_classe g in
  let perso = Personnage.init_perso n g c in
  Personnage.afficher_infos_perso perso
;;

