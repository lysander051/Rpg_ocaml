open Objet;;
open Monstre;;
open Personnage;;

module type GESTIONAVENTURE_SIG = 
sig
  exception Quitte_le_jeu
  val init_aventure : unit -> Personnage.perso
  val hubAventure : Personnage.perso -> unit
  val fin_partie : string -> unit
end;;

module GestionAventure : GESTIONAVENTURE_SIG=
struct

	(**
		exception levée quand le joueur quitte l'aventure
		@auteur 
	*)
  exception Quitte_le_jeu;;

	(**
		Delimiteur de ligne pour chaque nouveau message de l'aventure
		@auteur 
    @return un string de delimitation
	*)
  let delimiteur : unit -> string = fun () ->"\n+--------------------------------------------------------------------------------+\n"

	(**
		Permet de vérifier que le nom est correcte
		@auteur 
    @return le nom valide du joueur
	*)
  let rec read_nom  : unit -> string= fun () ->
    let () = print_string (delimiteur() ^ ">Ton nom: ") in
    let n = read_line() in
      if n="" then 
        (print_string "Il faut choisir un nom de personnage.\n"; read_nom())
      else 
        if (String.length n) > 40 then
          (print_string "Le nom est trop long.\n"; read_nom())
        else 
          n

  (**
		Transforme le choix du joueur de genre en constructeur de genre de personnage
		@auteur 
    @return le genre du personnage
	*)
  let rec read_genre : unit -> Personnage.genre= fun () ->  
    let () = print_string (delimiteur() ^ 
"> Ton genre: 
  F) Femme
  H) Homme
Votre choix: ") 
    in let g=read_line() in
    if not(g="F" || g="H" || g="f" || g="h") then 
        (print_string "tu dois avoir un genre binaire\n"; read_genre())
    else 
      (if g="F" || g="f" then Personnage.Femme else Personnage.Homme)

  (**
		Transforme le choix du joueur de classe en constructeur de classe de personnage
		@auteur 
    @param g permet d'afficher un message adapté au genre du personnage
    @return la classe du joueur
	*)
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
    in let c =read_line() in
      if not(c="A" || c="G" || c="M" || c="a" || c="g" || c="m") then 
        (print_string "il faut choisir une classe\n"; read_classe(g))
      else 
        (if c="A" || c="a" then 
          Personnage.Archer 
        else if c="G" || c="g"then 
          Personnage.Guerrier 
        else 
          Personnage.Magicien)

	(**
		Vérifie la validité du choix du joueur pour le hub d'aventure
		@auteur 
    @return un choix valide 
	*)
  let rec read_hubAventure : unit -> string= fun () ->
    let () = print_string (delimiteur() ^
"> Que voulez-vous faire?
 C) Continuer votre chemin
 D) Dormir
 M) Manger
 V) Visualiser l'état du personnage
 Q) Quitter l'aventure
Votre choix:") 
    in let c = read_line() in
      if not(c="C" || c="D" || c="M" || c="V" || c="Q" || c="c" || c="d" || c="m" || c="v" || c="q") then
        (print_string "il faut faire un choix\n"; read_hubAventure())
      else 
        c

	(**
		initialise le personnage et affiche le message de début d'aventure
		@auteur 
    @return le personnage de debut d'aventure
	*)
  let rec init_aventure : unit -> Personnage.perso = fun ()->
    let() = print_string ("\n\n\n\n" ^
    delimiteur()^
"> Bonjour jeune aventurier, es-tu prêt à mour... gagner. 
Pour ce faire ton but est simple. 
Deviens la personne la plus expérimentée et accumule des 
objets hors du commun.
  
Au fait qui es-tu aventurier?\n") in
    let n = read_nom() in
    let g = read_genre() in
    let c = read_classe g in
    Personnage.init_perso n g c

  (**
		Gestion du combat entre le joueur et un monstre
		@auteur 
    @param pers le personnage qui va combattre
    @param monstre le monstre qui va combattre
    @return le nouvel état du joueur
	*)
  let combattre : Personnage.perso -> Monstre.monstre -> Personnage.perso = fun pers monstre ->
    let rec le_combat :int -> Personnage.perso -> Monstre.monstre -> Personnage.perso = fun attaquant p m -> 
      match attaquant  with 
        |0 -> let frappe=Personnage.frapper p 
              in Personnage.affiche_attaque p frappe ; 
              let pv_monstre= m.pv - frappe  
              in
                if (pv_monstre <=0 ) then 
                  let nouv_xp= p.xp +( Monstre.xp_gagne monstre) in Monstre.monstre_vaincu m ;
                  Personnage.modifier_sac monstre.loot 1 (Personnage.changement_niveau p nouv_xp) 
                else 
                  let nouv_monstre : Monstre.monstre = {creature = monstre.creature; loot = monstre.loot; pv = pv_monstre}
                  in le_combat 1 p nouv_monstre
        |_->  let degat = (Monstre.monstre_frapper m) 
              in (print_string (Monstre.message_combat m degat)) ; 
              let nouv_pers=(Personnage.mis_a_jour_pv (-. degat ) p) 
              in le_combat 0 nouv_pers m
    in le_combat (Random.int 2) pers monstre

	(**
		Génére une rencontre avec un monstre aléatoirement
		@auteur 
    @param pers le personnage principal
	*)
  let malheureuse_rencontre : Personnage.perso -> Personnage.perso= fun perso->
    let rand = Random.int 100 in
      if rand < 50 then 
        let monstre = Monstre.init_monstre() in
        let () = print_string (delimiteur() ^ Monstre.message_malheureuse_rencontre monstre)
        in (combattre perso monstre)
      else 
        perso

	(**
		affiche les hub de l'aventure avec les différents choix
		@auteur 
    @param perso le personnage principal
	*)
  let rec hubAventure : Personnage.perso -> unit = fun perso ->
    let c = read_hubAventure() in
      match c with 
      | _ when c ="C" || c="c" -> 
        (print_string (delimiteur() ^ "> Vous continuez votre chemin vers votre prochaine destination.\n"); 
        hubAventure (malheureuse_rencontre perso))
      | _ when c="D" || c="d" -> 
        (print_string (delimiteur() ^ "> Vous installez votre campement et tombez rapidement endormie.\n"); 
        hubAventure (malheureuse_rencontre (Personnage.dormir perso)))
      | _ when c="M" || c="m" -> 
        (let mange = Personnage.manger perso in 
          if (fst mange) then 
            (print_string (delimiteur() ^ "> Vous mangez un peu avant de reprendre votre aventure.\n"); 
            hubAventure (malheureuse_rencontre (snd mange)))
          else 
            (print_string (delimiteur() ^ "> Vous n'avez pas à manger\n"); 
            hubAventure (malheureuse_rencontre (snd mange))))
      | _ when c="V" || c="v" -> 
        (print_string (delimiteur()); 
        Personnage.afficher_infos_perso perso; 
        hubAventure perso)
      |_ -> 
        raise Quitte_le_jeu

	(**
		Affiche la raison de la fin de partie
		@auteur 
    @param message le message de fin de partie
	*)
  let fin_partie : string -> unit = fun message ->
    print_string ("\n\n+---------------------------------Fin de partie----------------------------------+ \n" ^
    "> La partie s'est terminé car: \n" ^
    message)

end;;
