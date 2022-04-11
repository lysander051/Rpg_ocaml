open Objet;;
open Monstre;;
open Personnage;;

exception Tue_En_Dormant of Monstre.monstre ;;
exception Mort ;;

let dormir : Personnage.perso -> Personnage.perso = 
    fun perso -> let chance_monstre = Random.int 100 in
    if (chance_monstre<5) then let lemonstre = Monstre.init_monstre in raise (Tue_En_Dormant lemonstre)
    else 
      let nouv_pv= Personnage.mis_a_jour_pv(perso.pv +. 4.) in 
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
else (if g="F" then Personnage.Femme else Personnage.Homme);;

let rec read_classe = fun g ->
  let () = 
  if g = Personnage.Homme then 
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
  let c =read_line() in
  if not(c="A" || c="G" || c="M") then (print_string "il faut choisir une classe\n\n"; read_classe(g))
  else (if c="A" then Personnage.Archer else if c="G" then Personnage.Guerrier else Personnage.Magicien)
;;

let rec read_action = fun() ->
  let () = print_string "Que voulez-vous faire\n A) Attaquer \n F) Fuir \n V) Voir l'état de votre perso" in
  let c = read_line() in
  if not(c="A" || c="F" || c="V") then (print_string "il faut faire un choix\n\n"; read_action())
  else return c
;;

let rec init_aventure = fun ()->
  let n = read_nom() in
  let g = read_genre() in
  let c = read_classe g in
  let perso = Personnage.init_perso n g c in
  Personnage.afficher_infos_perso perso
;;

let malheureuse_rencontre = fun ()->
  let monstre = Monstre.init_monstre in
  let () = 
  if monstre.creature = Monstre.Golem then print_string "Le sol tremble sous vos pied, vous êtes destabilisé quand soudain un golem apparait devant vous.\n"
  else if monstre.creature = Monstre.Sanglier then print_string "Une odeur forte que vous connaissez bien, vous parvient. Un sanglier sort des bois et vous attaque.\n"
  else print_string "Vous entendez un bourdonnement tout autour de vous. quand soudain une nué de moustique se jette sur vous.\n"
  in
  let choix = read_action() in
  let rec aux = fun () ->
    if choix = "A" then combattre monstre perso
    else if choix = "F" then fuir monstre perso
    else (Personnage.afficher_infos_perso; aux())
  aux()
  ;;
let combattre : Personnage.perso -> Monstre.monstre ->Personnage.perso = fun p m ->
  let couple = if Random.int 2 = 0 then (p,m) else (m,p) in
  let rec le_combat = fun duo pers monstre -> 
    match fst duo with 
      |p when p = pers -> let pv_monstre= monstre.pv - Personnage.frapper p in 
        if pv_monstre <=0 then let nouv_xp=p.xp+Monstre.xp_gagne monstre in  
        let couple_niveau = Personnage.changement_niveau p.niveau nouv_xp in
        {nom : p.nom ; sexe : p.sexe ; role : p.role ; pv : p.pv ; xp :snd couple_niveau  ; niveau : fst couple_niveau  ; sac : p.sac
        else let nouv_monstre =  {creature :monstre.creature; loot : monstre.loot; pv : pv_monstre}
      in le_combat (nouv_monstre,p) p nouv_monstre

      |m when m=monstre -> let pv_pers = pers.pv - Monstre.monstre_frapper m in 
        if pv_pers <=0 then raise Mort
        else let nouv_pers = {  nom : pers.nom ; sexe : pers.sexe ; role : pers.role ; pv : pv_pers ; xp :pers.xp  ; niveau : pers.niveau  ; sac : pers.sac}
      in le_combat (nouv_pers,m) nouv_pers m
    in le_combat couple p m
    
  
