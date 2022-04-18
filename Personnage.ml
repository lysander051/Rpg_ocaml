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
  exception Objet_insuffisant of Objet.type_obj
  
  let classe_genre : perso -> string = fun perso -> match (perso.sexe, perso.role) with
    | (Homme, Archer) -> "Archer"
    | (Homme, Guerrier) -> "Guerrier"
    | (Homme, Magicien) -> "Magicien" 
    | (Femme, Archer) -> "Archère"
    | (Femme, Guerrier) -> "Guerrière"
    | (Femme, Magicien) -> "Magicienne"

  let init_perso: string -> genre -> classe -> perso = fun n -> fun g -> fun r ->
    {nom = n; sexe = g; role = r; pv = 20.; xp = 0; niveau = 1; sac = [] }

let string_of_pv : perso -> string = fun p ->
  let pv=(string_of_float p.pv) in 
  if p.pv <10. then "0"^pv
  else pv

let string_of_xp : perso -> string = fun p ->
  let xp = (string_of_int p.xp) in 
  if p.xp<10 then "0"^xp
  else xp

  let  etat_sac : perso -> string = fun perso ->
    let rec aux : sac -> string = fun sac ->
    match sac with
    | [] -> "" 
    | hd::tl when hd.type_obj=Rien ->""^aux tl
    | hd::tl -> ((string_of_int hd.qte) ^ "  " ^(Objet.affiche_objet hd.type_obj hd.qte)) ^ "\n" ^ aux tl
    in aux perso.sac

    let nb_string = fun st->
      let rec auxi : int ->string -> int= fun fois  s ->
        let len = String.length s in
        match s with
        |"" -> 0
        | _ ->(let debut = (String.sub s 0 1) in
        let verif=(String.escaped debut) in
        if debut=verif then 0 + ( auxi 0 (String.sub s 1 (len-1) ))
        else 
         (if fois=0
          then
          1+(auxi (fois+1) (String.sub  s 1 (len-1)))
      else
        0+(auxi (0) (String.sub  s 1 (len-1)))))
       in
        ((String.length st) - (auxi 0 st));;
  
let etat_perso = fun perso ->
  let debut= "|  " in
  let fin="  |\n" in 
  let premiere_ligne = 
   debut ^ perso.nom ^ "  |  " ^ (classe_genre perso) ^ "  niveau  " ^ (string_of_int(perso.niveau)) ^ fin in
    let reference = nb_string premiere_ligne in
    let delimitateur = "+"^ (String.make (reference-2) '-') ^"+\n"
  in   
    let make_ligne = fun reference debut fin ligne point -> 
      (let debut_ligne = debut ^ ligne in 
      let taille_ligne = nb_string (debut_ligne^point^fin) in
      let nb_espace = reference - taille_ligne in 
      debut_ligne ^( String.make nb_espace ' ' )^ point ^ fin )
    in 
    let pv="Points de vie  |" in
    let experience = make_ligne (String.length pv) "" "  |" "Expérience" "" in 
    delimitateur^premiere_ligne ^delimitateur^
    (make_ligne reference debut fin pv (string_of_pv perso) ) ^ delimitateur^
    (make_ligne reference debut fin experience (string_of_xp perso) ) ^ delimitateur^
    (make_ligne reference debut fin "Sac" "" ) ^
    
    let rec chq_ligne_sac = fun reference debut fin s ->
      let len_s= String.length s in
      match s with 
      |""-> ""
        | _ -> let indice= String.index s '\n' in
        let un_objet = String.sub s 0 (indice) in
        (make_ligne reference (debut^"  ") fin un_objet "" ) ^ 
        (chq_ligne_sac  reference debut fin ( String.sub s (indice+1) (len_s-indice-1)))
    in
   ( chq_ligne_sac reference debut fin  (etat_sac perso)) ^ delimitateur

   let afficher_infos_perso= fun perso -> print_string ( etat_perso perso)
   let afficher_sac_perso= fun perso -> print_string ( etat_sac perso)
  
 (* let vie_perso = fun perso -> 
    if perso.pv = 1. || perso.pv = 2. || perso.pv = 3. || perso.pv = 4. || perso.pv = 5. || perso.pv = 6. || perso.pv = 7. || perso.pv = 8. || perso.pv = 9.
      then "0" ^ string_of_float(perso.pv) ^ "/20." ^ " |"
      else if perso.pv = 11. || perso.pv = 12. || perso.pv = 13. || perso.pv = 14. || perso.pv = 15. || perso.pv = 16. || perso.pv = 17. || perso.pv = 18. || perso.pv = 19. || perso.pv = 20.
           then string_of_float(perso.pv) ^ "/20." ^ " |"
           else if perso.pv <= 10. then string_of_float(perso.pv) ^ "/20." ^ " |"
           else string_of_float(perso.pv) ^ "/20." ^ "|" *)
        
  let nb_degats = fun perso -> match perso.role with

    | Archer -> "4"
    | Guerrier -> "10"
    | Magicien -> "5"     
      
 (* let chance_toucher = fun perso niveau -> match (perso.role, perso.niveau) with
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
  *)
 

  let mis_a_jour_pv : float -> perso -> perso = fun ajoutPv-> fun perso ->
    if perso.pv +. ajoutPv > 20. 
      then 
        {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = 20.; xp = perso.xp; niveau = perso.niveau; sac = perso.sac }
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
      | h::t -> (*false ||*) aux t
    in aux pers.sac

  let modifier_sac : Objet.type_obj -> int -> perso -> perso = fun t_obj n perso ->
    if t_obj = Rien then perso
    else 
    let rec aux :  sac -> sac -> perso= fun(* t_obj n *) nouveauSac persoSac ->
      match persoSac with
      | [] when n<=0 -> {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = perso.pv; xp = perso.xp; niveau = perso.niveau; sac = nouveauSac }
      | [] ->  {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = perso.pv; xp = perso.xp; niveau = perso.niveau; sac = (nouveauSac @ [{type_obj = t_obj; qte = n}]) }
      | {type_obj=a; qte=b}::t when a=t_obj && b=(-n) -> aux(* t_obj n *) (nouveauSac) t
      |{type_obj=a; qte=b}::t when a=t_obj && b<(-n) -> raise (Objet_insuffisant a)

     | {type_obj=a; qte=b}::t when a=t_obj && b>(-n) -> let le_sac = (nouveauSac @ ({type_obj=a; qte=b+n}:: t) ) in
     {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = perso.pv; xp = perso.xp; niveau = perso.niveau; sac = le_sac }
      | h::t -> aux (*t_obj n*)  (nouveauSac @ [h]) t
    in aux(* t_obj n*) [] (perso.sac)
  

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

 (* let changement_niveau : perso -> int -> perso = fun p pt_xp ->
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
      in aux p.niveau pt_xp  *)

      (* A NE PAS EFFACER*)
 let changement_niveau : perso -> int -> perso = fun p xp ->
  let rec aux : int -> int -> perso= fun le_xp le_niveau ->
    let xp_final_du_niveau =int_of_float( (2. ** float(le_niveau) )*. 10. )in 
    if le_niveau = 10 then
      raise LevelMax
    else 

    if le_xp < xp_final_du_niveau then
      {nom = p.nom ; sexe = p.sexe; role = p.role; pv = p.pv; xp = le_xp; niveau =le_niveau ; sac = p.sac }
    else
      let nouv_xp = le_xp - xp_final_du_niveau in 
      let nouv_niveau = le_niveau +1 in 
      aux nouv_xp nouv_niveau
    in aux xp p.niveau
    

  let affiche_attaque :perso -> int -> unit = fun p frappe ->    
    match frappe with 
    | 0 -> ( print_string "Vous portez une attaque, mais vous manquez votre cible \n")
    | _ -> ( print_string ("Vous frappez et infligez "^  (nb_degats p) ^ " points de dégât \n"))
    (* | _ -> ( print_string  "Vous frappez et infligez " ^  (nb_degats p) ^ "points de dégât")*)

end;;

