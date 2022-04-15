open Objet;;
open Monstre;;

module Personnage = 
struct 

  type classe = Archer | Guerrier | Magicien
  type genre = Homme | Femme
  type objet={type_obj : Objet.type_obj ; qte : int}
  type sac= objet list
  type perso = { nom : string ; sexe : genre ; role : classe ; pv : float ; xp :int  ; niveau : int  ; sac : sac}
  
  exception Personnage_mort
  exception LevelMax
  exception Tue_En_Dormant of Monstre.monstre 
  
  let classe_genre = fun perso -> match (perso.sexe, perso.role) with
    | (Homme, Archer) -> "Archer"
    | (Homme, Guerrier) -> "Guerrier"
    | (Homme, Magicien) -> "Magicien" 
    | (Femme, Archer) -> "Archere"
    | (Femme, Guerrier) -> "Guerriere"
    | (Femme, Magicien) -> "Magicienne"

  let init_perso = fun n -> fun g -> fun r ->
    {nom = n; sexe = g; role = r; pv = 20.; xp = 0; niveau = 1; sac = [] }
  

  let vie_perso = fun perso -> 
    if perso.pv = 1. || perso.pv = 2. || perso.pv = 3. || perso.pv = 4. || perso.pv = 5. || perso.pv = 6. || perso.pv = 7. || perso.pv = 8. || perso.pv = 9.
      then "0" ^ string_of_float(perso.pv) ^ "/20." ^ " |"
      else if perso.pv = 11. || perso.pv = 12. || perso.pv = 13. || perso.pv = 14. || perso.pv = 15. || perso.pv = 16. || perso.pv = 17. || perso.pv = 18. || perso.pv = 19. || perso.pv = 20.
           then string_of_float(perso.pv) ^ "/20." ^ " |"
           else if perso.pv <= 10. then string_of_float(perso.pv) ^ "/20." ^ " |"
           else string_of_float(perso.pv) ^ "/20." ^ "|"
        
  let nb_degats = fun perso -> match perso.role with
    | Archer -> "04"
    | Guerrier -> "10"
    | Magicien -> "05"     
      
  let chance_toucher = fun perso niveau -> match (perso.role, perso.niveau) with
    | (Archer, n) -> if n > 2 then string_of_int(70+5*perso.niveau) else "70"
    | (Guerrier, n) -> if n > 2 then string_of_int(30+5*perso.niveau) else "30" 
    | (Magicien, n) -> if n > 2 then string_of_int(50+5*perso.niveau) else "50"
      
  let lvl_sup = fun perso -> if ((2.**float(perso.niveau))*.10.)-.float(perso.xp) > 100. || ((2.**float(perso.niveau))*.10.)-.float(perso.xp) < 0.
      then "Niveau supérieur   : " ^ string_of_int(int_of_float(((2.**float(perso.niveau))*.10.)-.float(perso.xp))) ^ "     |"
      else if ((2.**float(perso.niveau))*.10.)-.float(perso.xp) > 10. then "Niveau supérieur   : " ^ string_of_int(int_of_float(((2.**float(perso.niveau))*.10.)-.float(perso.xp))) ^ "      |"
      else "Niveau supérieur   : 0" ^ string_of_int(int_of_float(((2.**float(perso.niveau))*.10.)-.float(perso.xp))) ^ "      |"


  let rec presence_objet : objet list -> (objet list * string) = fun objets -> 
    match objets with
    | [] -> ([], "                  ")
    | hd::tl -> (tl, Objet.visuel_objet hd.type_obj hd.qte)
    
  let experience_perso = fun perso -> if perso.xp > 99 
    then "Expérience         : " ^ string_of_int perso.xp ^ "     | "
    else if perso.xp > 9 then "Expérience         : " ^ string_of_int perso.xp ^ "      | "
    else "Expérience         : " ^ "0" ^ string_of_int perso.xp ^ "      | "

  let rec iterate : (int * ('a->'a) * 'a) -> 'a =
    fun (count, f, initial_value) ->
      if count <= 0
      then initial_value
      else iterate (count-1, f, f initial_value)
  let ligne = fun perso -> String.length ("| " ^ perso.nom ^ " | " ^ (classe_genre perso) 
                                          ^ "  Niveau " ^ string_of_int (perso.niveau) ^ "\n")   
  let repeat_string = fun (s,ligne) -> iterate (ligne+1,( fun p -> p ^ s) , "")
  
  let affichage_ligne = fun chaine -> "+" ^ (repeat_string ("-", chaine)) ^ "+"
  let affichage_fiche_perso = fun chaine -> chaine 
  let affichage_attr_perso = fun chaine -> chaine
  let _enclosing = fun chaine -> "| " ^ chaine 
  let fermeture = fun chaine -> repeat_string(" ", String.length("+----------------- Fiche de personnage --------------+")-String.length(chaine)-4) ^ "|"
  let fermer_tableau = fun chaine -> repeat_string(" ", String.length("+----------------- Fiche de personnage --------------+")-String.length(chaine)-3) ^ "|"
    

  let etat_perso = fun perso -> 
    let sac = (presence_objet perso.sac) in
    affichage_fiche_perso ("+---------------- Fiche de personnage ---------------+") ^ "\n" ^ 
    _enclosing("Nom : " ^ perso.nom) ^ (fermeture ("Nom : " ^ perso.nom)) ^
    repeat_string (" ", String.length("Niveau " ^ string_of_int (perso.niveau) ^ " Classe : " ^ (classe_genre perso))*2+1+String.length(perso.nom) - (String.length("Niveau " ^ string_of_int (perso.niveau) ^ " Classe : " ^ (classe_genre perso))- String.length(perso.nom))) ^ "\n" ^ 
    _enclosing("Niveau " ^ string_of_int (perso.niveau) ^ "        Classe : " ^ (classe_genre perso)) ^ (fermeture ("Niveau " ^ string_of_int (perso.niveau) ^ "        Classe : " ^ (classe_genre perso))) ^
    repeat_string (" ", String.length("Niveau " ^ string_of_int (perso.niveau) ^ " Classe : " ^ (classe_genre perso))-5) ^ "\n" ^ 
    
    
    affichage_attr_perso("+---------- Attribut ------------------- Sac --------+") ^ "\n" ^ 
    
    _enclosing("Point de vie       : " ^ (vie_perso perso) ) ^ (snd sac) ^ " " ^ fermer_tableau("Point de vie :         " ^ (vie_perso perso) ^ (snd sac))  ^ 

    let sac = (presence_objet (fst sac)) in
    repeat_string(" ", String.length("Niveau " ^ string_of_int (perso.niveau) ^ " Classe : " ^ (classe_genre perso))*2-String.length("Point de vie : " ^ (string_of_float perso.pv))-2) ^ "\n" ^
    _enclosing("Dégats             : " ^ (nb_degats (perso) ^ "      |")) ^ (snd sac) ^ fermer_tableau("Dégats :             " ^ (nb_degats (perso) ^ "      |") ^ (snd sac)) ^
    
    let sac = (presence_objet (fst sac)) in
    repeat_string(" ", String.length("Niveau " ^ string_of_int (perso.niveau) ^ " Classe : " ^ (classe_genre perso))*2-String.length("Dégats :            " ^ (nb_degats (perso)))-2) ^ "\n" ^
    _enclosing("Chances de toucher : " ^ (chance_toucher perso perso.niveau) ^ "      |") ^ (snd sac) ^ " "  ^ fermer_tableau("Chances de toucher :  " ^ (chance_toucher perso perso.niveau) ^ "      |" ^ (snd sac)) ^
    
    let sac = (presence_objet (fst sac)) in 
    repeat_string(" ", String.length("Niveau " ^ string_of_int (perso.niveau) ^ " Classe : " ^ (classe_genre perso))*2-String.length("Chances de toucher : " ^ (chance_toucher perso perso.niveau))-2) ^ "\n" ^
    _enclosing(experience_perso perso) ^ (snd sac) ^ fermer_tableau(experience_perso perso ^ (snd sac) ) ^

    let sac = (presence_objet (fst sac)) in 
    repeat_string(" ", String.length("Niveau " ^ string_of_int (perso.niveau) ^ " Classe : " ^ (classe_genre perso))*2-String.length("Expérience :          " ^ (string_of_int perso.xp )^ "      |")-2) ^ "\n" ^ 
    _enclosing( lvl_sup perso ) ^ (snd sac) ^ fermer_tableau(lvl_sup perso ^ (snd sac)) ^ "\n" ^
    
    
    affichage_ligne(String.length("+---------------- Fiche de personnage ------------+")) ^ "\n" 


  let afficher_infos_perso = fun perso -> print_string(etat_perso perso)

  let mis_a_jour_pv = fun ajoutPv-> fun perso ->
    if perso.pv +. ajoutPv > 20. then {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = 20.; xp = perso.xp; niveau = perso.niveau; sac = perso.sac }
    else 
      if(* perso.pv +. ajoutPv < 20. && *)perso.pv +. ajoutPv > 0. then {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = perso.pv+.ajoutPv; xp = perso.xp; niveau = perso.niveau; sac = perso.sac }
      else raise Personnage_mort

  let frapper : perso -> int = fun perso ->
    let chance = Random.int 100 in  let add_bonus=5*((perso.niveau) -1 ) in
    match perso.role with 
    | Archer when chance <70 + add_bonus -> 4
    | Magicien when chance <50 +add_bonus ->5
    | Guerrier when chance < 30 + add_bonus ->10
    | _ -> 0
    

  let avoir_un_poulet : perso -> bool = fun pers ->
    let rec aux = fun sac -> 
      match sac with
      | [] -> false
      | {type_obj=a; qte=b}::_ when a=Poulet && b>0 -> true
      | h::t -> false || aux t
    in aux pers.sac

  let modifier_sac = fun t_obj n perso ->
    let rec aux = fun t_obj n nouveauSac persoSac ->
      match persoSac with
      | [] when n<=0 -> {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = perso.pv; xp = perso.xp; niveau = perso.niveau; sac = nouveauSac }
      | [] -> {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = perso.pv; xp = perso.xp; niveau = perso.niveau; sac = {type_obj = t_obj; qte = n}::nouveauSac }
      | {type_obj=a; qte=b}::t when a=t_obj && b=(-n) -> aux t_obj n (nouveauSac) t
      | {type_obj=a; qte=b}::t when a=t_obj && b>(-n) -> aux t_obj n ({type_obj=a; qte=b+n}::nouveauSac) t 
      | h::t -> aux t_obj n (h::nouveauSac) t
    in aux t_obj n [] perso.sac

  let manger : perso -> (bool *perso) = fun perso ->
    if (not(avoir_un_poulet perso) )  then (false,perso)
    else (true, (modifier_sac (Objet.Poulet) (-1) (mis_a_jour_pv 2. perso)))

  let dormir : perso -> perso = 
    fun perso -> let chance_monstre = Random.int 100 in
    if (chance_monstre<5) 
      then let lemonstre = Monstre.init_monstre() in 
      ((print_string ("Malheureusement, vous vous faites attaquer dans la nuit.\n" ^ Monstre.nom_monstre lemonstre ^ " vous attaque et vous êtes mort.\n")); 
      raise (Tue_En_Dormant lemonstre))
    else 
      print_string ("Vous passez une nuit revigorante et êtes prêt à reprendre l'aventure... \nOu redormir\n");
      mis_a_jour_pv 4. perso

 (* let rec changement_niveau :int -> float -> int*int = fun niv xp  ->
    let niv_1 = (2.**float(niv))*.10. in let niv_2= (2.**float(niv+1))*.10. in 
    if xp >= niv_2 then changement_niveau (niv+1) xp
    else let nouv_xp = xp -. niv_1 in (niv,(int_of_float nouv_xp))*)

  let changement_niveau : perso -> int -> perso = fun p pt_xp ->
    let rec aux : int -> int -> perso = fun niv xp  ->
      let niv_1 = (2.**float(niv))*.10. in let niv_avant= (2.**float(niv-1))*.10. in 
      if (float)xp >= niv_1 then 
        if 10 <= (niv+1) then raise LevelMax
        else aux (niv+1) xp 
      else 
        if niv=1 then  {nom = p.nom ; sexe = p.sexe; role = p.role; pv = p.pv; xp = pt_xp; niveau =niv ; sac = p.sac }
        else
          let nouv_xp =xp - (int_of_float niv_avant) in 
          ( if niv != p.niveau then (print_string ("Vous avez atteint le niveau " ^ (string_of_int niv)))) ;
          {nom = p.nom ; sexe = p.sexe; role = p.role; pv = p.pv; xp = nouv_xp; niveau =niv ; sac = p.sac }
      in aux p.niveau pt_xp

  let affiche_attaque :perso -> int -> unit = fun p frappe ->    
    match frappe with 
    | 0 -> ( print_string "Vous ripostez, mais vous manquez la cible \n")
    | _ -> ( print_string ("Vous frappez et infligez "^  (nb_degats p) ^ " points de dégât \n"))
    (* | _ -> ( print_string  "Vous frappez et infligez " ^  (nb_degats p) ^ "points de dégât")*)

end;;

