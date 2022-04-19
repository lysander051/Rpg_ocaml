open Objet;;
open Monstre;;

module type PERSONNAGE_SIG =
sig
  type classe = Archer | Guerrier | Magicien
  type genre = Homme | Femme
  type objet
  type sac 
  type perso = { nom : string ; sexe : genre ; role : classe ; pv : float ; xp :int  ; niveau : int  ; sac : sac}

  exception Personnage_mort
  exception LevelMax
  exception Tue_En_Dormant of Monstre.monstre 
  exception Objet_insuffisant of Objet.type_obj

  val init_perso : string -> genre -> classe -> perso
  val frapper : perso -> int
  val affiche_attaque :perso -> int -> unit
  val modifier_sac : Objet.type_obj -> int -> perso -> perso
  val changement_niveau : perso -> int -> perso
  val mis_a_jour_pv : float -> perso -> perso
  val dormir : perso -> perso
  val manger : perso -> (bool *perso)
  val afficher_infos_perso : perso -> unit
end;;



module Personnage : PERSONNAGE_SIG = 
struct 
  (**
    La classe du personnage peut être un archer, un guerrier ou un magicien
    @auteur
  *)
  type classe = Archer | Guerrier | Magicien
  (**
    Le personnage peut être un homme ou une femme
    @auteur
  *)
  type genre = Homme | Femme
  (**
    Un objet est une certaine quantité d'un type d'objet
    @auteur
  *)
  type objet = {type_obj : Objet.type_obj ; qte : int}
  (**
    Un sac peut contenir zéro à plusieurs objets
    @auteur
  *)
  type sac = objet list
  (**
  Un personnage possède un nom, un genre, une classe, un point de vie, un point d'expérience et un sac
  @auteur
  *)
  type perso = { nom : string ; sexe : genre ; role : classe ; pv : float ; xp :int  ; niveau : int  ; sac : sac}
  
  (**
    Une exception quand le personnage est mort
    @auteur
  *)
  exception Personnage_mort
  (**
    Une exception quand le personnage atteint le niveau 10
    @auteur
  *)
  exception LevelMax
  (**
    Une exception quand le personnage est tué quand il dort
    par un monstre
    @auteur
  *)
  exception Tue_En_Dormant of Monstre.monstre 
  (**
    Une exception quand le personnage n'a pas assez d'objet 
  @auteur
  *)
  exception Objet_insuffisant of Objet.type_obj
  
  (**
    La classe du personnage selon son genre
    @auteur
    @param perso le personnage dont on veut savoir la classe
    @return le string de la classe selon le genre du personnage
  *)
  let classe_genre : perso -> string = fun perso -> match (perso.sexe, perso.role) with
    | (Homme, Archer) -> "Archer"
    | (Homme, Guerrier) -> "Guerrier"
    | (Homme, Magicien) -> "Magicien" 
    | (Femme, Archer) -> "Archère"
    | (Femme, Guerrier) -> "Guerrière"
    | (Femme, Magicien) -> "Magicienne"

  (**
    Initialisation d'un personnage 
    @auteur
    @param n le nom du personnage
    @param g le genre du personnage
    @param r le rôle du personnage
    @return un personnage
  *)
  let init_perso : string -> genre -> classe -> perso = fun n -> fun g -> fun r ->
    {nom = n; sexe = g; role = r; pv = 20.; xp = 0; niveau = 1; sac = [] }

  (**
    L'affichage du point de vie du personnage
    @auteur
    @param p le personnage dont on veut afficher le point de vie
    @return un string du point de vie du personnage
  *)
  let string_of_pv : perso -> string = fun p ->
  let pv=(string_of_float p.pv) in 
  if p.pv <10. then "0" ^ pv
  else pv

  (**
    L'affichage du point d'experience du personnage
    @auteur
    @param p le personnage dont on veut afficher le point d'expérience
    @return un string du point d'expérience du personnage
  *)
  let string_of_xp : perso -> string = fun p ->
  let xp = (string_of_int p.xp) in 
  if p.xp<10 then "0" ^ xp
  else xp

  (**
    L'état du sac du personnage
    Si le sac n'est pas vide alors tous les objets contenus dans le sac seront pris en compte
    @auteur
    @param perso le personnage possédant le sac
    @return un texte d'affichage de l'état du sac
  *)
  let  etat_sac : perso -> string = fun perso ->
    let rec aux : sac -> string = fun sac ->
      match sac with
        | [] -> "" 
        | hd::tl when hd.type_obj=Rien ->""^aux tl
        | hd::tl -> ((string_of_int hd.qte) ^ "  " ^(Objet.affiche_objet hd.type_obj hd.qte)) ^ "\n" ^ aux tl
    in aux perso.sac

  (**
    La longueur d'un string même avec n'importe quelle caractère
    @auteur
    @param st le string dont on veut avoir la longueur 
    @return la longueur d'un string
  *)
    let nb_string = fun st->
      let rec auxi : int ->string -> int= fun fois  s ->
        let len = String.length s in
          match s with
          |"" -> 0
          | _ ->(let debut = (String.sub s 0 1) in
        let verif=(String.escaped debut) in
          if debut=verif then
             0 + ( auxi 0 (String.sub s 1 (len-1) ))
          else 
            (if fois=0  then
              1+(auxi (fois+1) (String.sub  s 1 (len-1)))
            else
              0+(auxi (0) (String.sub  s 1 (len-1)))))
      in
      ((String.length st) - (auxi 0 st));;
  
(**
  L'état du personnage contenant le nom , la classe , le niveau, 
  le point de vie, le point d'expérience et l'état du sac
  @auteur 
  @param perso le personnage dont on veut avoir l'état 
  @return un texte d'affichage de l'état du personnage

*)
let etat_perso : perso -> string = fun perso ->
  let debut= "|  " in
  let fin="  |\n" in 
  let premiere_ligne = 
    debut ^ perso.nom ^ "  |  " ^ (classe_genre perso) ^ "  niveau  " ^ (string_of_int(perso.niveau)) ^ fin in
  let reference = nb_string premiere_ligne in
  let delimitateur = "+"^ (String.make (reference-2) '-') ^"+\n" in   
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

  (**
    Affichage de l'état du personnage 
    @auteur
    @param perso le personnage dont on veut afficher l'état du personnage
  *)
   let afficher_infos_perso : perso -> unit = fun perso -> print_string ( etat_perso perso)

   (**
    Affichage de l'état du sac du personnage
    @auteur
    @param perso le personnage dont on veut afficher l'état de son sac
  *)
   let afficher_sac_perso = fun perso -> print_string ( etat_sac perso)
  
 (* let vie_perso = fun perso -> 
    if perso.pv = 1. || perso.pv = 2. || perso.pv = 3. || perso.pv = 4. || perso.pv = 5. || perso.pv = 6. || perso.pv = 7. || perso.pv = 8. || perso.pv = 9.
      then "0" ^ string_of_float(perso.pv) ^ "/20." ^ " |"
      else if perso.pv = 11. || perso.pv = 12. || perso.pv = 13. || perso.pv = 14. || perso.pv = 15. || perso.pv = 16. || perso.pv = 17. || perso.pv = 18. || perso.pv = 19. || perso.pv = 20.
           then string_of_float(perso.pv) ^ "/20." ^ " |"
           else if perso.pv <= 10. then string_of_float(perso.pv) ^ "/20." ^ " |"
           else string_of_float(perso.pv) ^ "/20." ^ "|" 
      
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
  *)
 
  (**
    Mise à jour du point de vie du personnage en ajoutant ou en déduisant un point de vie donnée
    si le point de vie est supérieur à 20 on reste à 20
    si le point de vie est inférieur ou égal à 0 alors le personnage est mort
    @auteur
    @param ajoutPv le point de vie donné à ajouter ou à déduire
    @param perso le personnage dont on veut mêtre à jour le point de vie
    @raise Personnage_mort quand le le point de vie du personnage est à 0
    @return un personnage avec le point de vie mis à jour
  *)
  let mis_a_jour_pv : float -> perso -> perso = fun ajoutPv-> fun perso ->
    if (perso.pv +. ajoutPv > 20.) then 
        {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = 20.; xp = perso.xp; niveau = perso.niveau; sac = perso.sac }
    else 
      if (perso.pv +. ajoutPv > 0.) then 
        {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = perso.pv+.ajoutPv; xp = perso.xp; niveau = perso.niveau; sac = perso.sac }
      else 
        raise Personnage_mort

  (**
        Le personnage frappe le monstre 
        Chaque classe a une certaine chance de toucher son cible 
        et selon son niveau un certain nombre de point de vie est déduit de son cible s'il arrive à le toucher
        @auteur
        @param perso le personnage qui frappe le monstre 
        @return le nombre de dégat que le personnage inflige au monstre
  *)
  let frapper : perso -> int = fun perso ->
    let chance = Random.int 100 in  
    let add_bonus=5*((perso.niveau) -1 ) in
      match perso.role with 
        | Archer when chance <70 + add_bonus -> 4
        | Magicien when chance <50 +add_bonus ->5
        | Guerrier when chance < 30 + add_bonus ->10
        | _ -> 0
  
  (**
    Pour savoir si le personnage possède un poulet
    @auteur
    @param pers le personnage dont on veut savoir s'il possède un poulet
    @return true si il a un poulet dans son sac false sinon
  *)
  let avoir_un_poulet : perso -> bool = fun pers ->
    let rec aux = fun sac -> 
      match sac with
        | [] -> false
        | {type_obj=a; qte=b}::_ when a=Poulet && b>0 -> true
        | h::t -> (*false ||*) aux t
    in aux pers.sac

  (**
    Pour modifier le sac du personnage
    c'est-à-dire qu'on ajoute ou on enlève un type d'objet dans le sac du personnage
    @auteur
    @param t_objet le type d'objet dont on veut ajouter ou enlever
    @param n la quantité de type d'objet dont on veut ajouter ou enlever
    @param perso le personnage dont on veut modifier le sac
    @raise Objet_insuffisant si le personnage ne possède pas assez du type d'objet dont on veut enlever
    @return le personnage avec le nouveau sac 
  *)
  let modifier_sac : Objet.type_obj -> int -> perso -> perso = fun t_obj n perso ->
    if t_obj = Rien then perso
    else 
      let rec aux :  sac -> sac -> perso= fun nouveauSac persoSac ->
        match persoSac with
          | [] when n<=0 -> {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = perso.pv; xp = perso.xp; niveau = perso.niveau; sac = nouveauSac }
          | [] ->  {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = perso.pv; xp = perso.xp; niveau = perso.niveau; sac = (nouveauSac @ [{type_obj = t_obj; qte = n}]) }
          | {type_obj=a; qte=b}::t when a=t_obj && b=(-n) -> aux (nouveauSac) t
          |{type_obj=a; qte=b}::t when a=t_obj && b<(-n) -> raise (Objet_insuffisant a)
          | {type_obj=a; qte=b}::t when a=t_obj && b>(-n) -> 
              let le_sac = (nouveauSac @ ({type_obj=a; qte=b+n}:: t) ) in
                {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = perso.pv; xp = perso.xp; niveau = perso.niveau; sac = le_sac }
          | h::t -> aux (nouveauSac @ [h]) t
      in aux [] (perso.sac)
  

  (**
      Le personnage mange un poulet de son sac et obtient 2 points de vie supplémentaire
      Si le personnage ne possède pas de poulet, il ne peut pas manger
      @auteur
      @param perso le personnage qui veut manger
      @return true et le personnage enlevé d'un poulet dans son sac ou false et le personnage initial
  *)
  let manger : perso -> (bool *perso) = fun perso -> 
    if (not(avoir_un_poulet perso) )  then 
      (false,perso)
    else 
      (true, (modifier_sac (Objet.Poulet) (-1) (mis_a_jour_pv 2. perso)))
    

  (**
      Le personnage dort et gagne 4 points de vie supplémentaire si aucun danger ne se passe durant sa nuit ou sa sieste
      Un monstre peut l'attaquer quand il dort 
      @auteur
      @param perso le personnage qui dort
      @raise Tue_En_Dormant monstre si le personnage est tué par le monstre durant son sommeil
      @return le personnage avec des points de vie en plus ou bien un message indiquant sa mort
  *)
  let dormir : perso -> perso = fun perso -> 
    let chance_monstre = Random.int 100 in
      if (chance_monstre<5) then 
        let lemonstre = Monstre.init_monstre() in 
        ((print_string ("Malheureusement, vous vous faites attaquer dans la nuit.\n" ^ Monstre.nom_monstre lemonstre ^ " vous attaque et vous êtes mort.\n")); 
        raise (Tue_En_Dormant lemonstre))
      else 
        print_string ("Vous passez une nuit revigorante et êtes prêt à reprendre l'aventure... \nOu redormir\n");
        mis_a_jour_pv 4. perso

  (**
      Le changement de niveau du personnage s'effectue si le point d'expérience est supérieur 
      au point d'expérience maximum de son niveau actuel
      Si il atteint le niveau 10 alors le jeu est fini
      @auteur
      @param p le personnage joué
      @param xp le nouveau point d'expérience du personnage 
      @raise LevelMax quand le personnage atteint le niveau 10
      @return le personnage avec peut être un augmentation de niveau ou bien un message indiquant la fin du jeu 
  *)
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

  (**
    Le nombre de dégats  que le personnage peut infliger à son adversaire selon sa classe 
    si il arrive à toucher sa cible
    @auteur
    @param p le personnage qui inflige le dégat
    @return le nombre de point de vie retirer à un monstre si jamais le personnage touche sa cible
  *)
  let nb_degats = fun p ->
    let add_bonus=5*((p.niveau) -1 ) in
    add_bonus +
    match p.role with
        | Archer -> 4 
        | Magicien->5
        | Guerrier ->10

    
  (**
    Affichage du message quand le personnage frappe 
    un message s'il manque sa cible
    et un autre s'il arrive à l'avoir
    @auteur
    @param p le personnage qui frappe
    @param frappe la frappe du personnage si c'est 0 alors il a manqué sa cible sinon il l'a eu
    
  *)
  let affiche_attaque :perso -> int -> unit = fun p frappe ->    
    match frappe with 
      | 0 -> ( print_string "Vous portez une attaque, mais vous manquez votre cible \n")
      | _ -> ( print_string ("Vous frappez et infligez "^  (string_of_int(nb_degats p)) ^ " points de dégât \n"))


end;;

