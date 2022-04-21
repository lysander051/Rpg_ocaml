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
 val message_attaque :perso -> int -> string
  val modifier_sac : Objet.type_obj -> int -> perso -> perso
  val changement_niveau : perso -> int -> perso
  val mis_a_jour_pv : float -> perso -> perso
  val dormir : perso -> perso
  val manger : perso -> (bool *perso)
  val afficher_infos_perso : perso -> unit
  val accord_masculin_feminin : perso -> string -> string -> string
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
    Une exception quand le personnage meurt
    @auteur
  *)
  exception Personnage_mort

  
  (**
    Une exception quand le personnage atteint le niveau 10
    @auteur
  *)
  exception LevelMax


  (**
    Une exception quand le personnage est tué  par un monstre quand il dort
   
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
  let classe_genre : perso -> string = fun perso -> 
    match (perso.sexe, perso.role) with
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
    {nom = n; sexe = g; role = r; pv = 20.; xp = 18; niveau = 1; sac = [] }


  (**
    L'affichage du point de vie du personnage
    @auteur
    @param p le personnage dont on veut afficher le point de vie
    @return un string du point de vie du personnage
  *)
  let string_of_pv : perso -> string = fun p ->
    let pv=(string_of_float p.pv) in 
      if p.pv <10. then 
        "0" ^ pv
      else 
        pv

        
  (**
    L'affichage d'un élément de type int
    @auteur
    @param element l'entier dont on veut afficher
    @return un string de l'élément 
  *)
  let string_of_element : int-> string = fun element ->
    let elmt = (string_of_int element) in 
      if element<10 then 
        "0" ^ elmt
      else 
        elmt


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
  let delimitation = "+"^ (String.make (reference-2) '-') ^"+\n" in   
  let make_ligne = fun reference debut fin ligne point -> 
      (let debut_ligne = debut ^ ligne in 
      let taille_ligne = nb_string (debut_ligne^point^fin) in
      let nb_espace = reference - taille_ligne in 
        debut_ligne ^( String.make nb_espace ' ' )^ point ^ fin )
  in 
  let pv="Points de vie  |" in
  let experience = make_ligne (String.length pv) "" "  |" "Expérience" "" in 
    delimitation^premiere_ligne ^delimitation^
    (make_ligne reference debut fin pv (string_of_pv perso) ) ^ delimitation^
    (make_ligne reference debut fin experience (string_of_element perso.xp) ) ^ delimitation^
    (make_ligne reference debut fin "Sac" "" ) ^
    if (etat_sac perso)="" then 
      (make_ligne reference (debut^"  ") fin "vide" "" ) ^ delimitation
    else
  let rec chq_ligne_sac = fun reference debut fin s ->
    let len_s= String.length s in
      match s with 
      |""-> ""
      | _ -> let indice= String.index s '\n' in
        let un_objet = String.sub s 0 (indice) in
        (make_ligne reference (debut^"  ") fin un_objet "" ) ^ 
        (chq_ligne_sac  reference debut fin ( String.sub s (indice+1) (len_s-indice-1)))
    in
   ( chq_ligne_sac reference debut fin  (etat_sac perso)) ^ delimitation


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
 
   
  (**
    Mise à jour du point de vie du personnage en ajoutant ou en déduisant un point de vie donnée
    si le point de vie est supérieur à 20 on reste à 20
    si le point de vie est inférieur ou égal à 0 alors le personnage est mort
    @auteur
    @param ajoutPv le point de vie donné à ajouter ou à déduire
    @param perso le personnage dont on veut mêtre à jour le point de vie
    @raise Personnage_mort quand le point de vie du personnage est à 0
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
    Le nombre de dégats que le personnage peut infliger à son adversaire selon sa classe 
    si il arrive à toucher sa cible
    @auteur
    @param p le personnage qui inflige le dégat
    @return le nombre de point de vie retirer à un monstre si jamais le personnage le touche 
  *)
  let nb_degats : perso -> int = fun p ->
    match p.role with
        | Archer -> 4 
        | Magicien->5
        | Guerrier ->10

    
  (** 
    Le pourcentage de chance que le personnage arrive à toucher son cible selon son niveau actuel quand il combat
    @auteur
    @param perso le personnage qui combat
      *)
  let chance_toucher : perso -> int = fun perso ->
    let chance =
      let add_bonus=5*((perso.niveau) -1 ) in
        add_bonus + 
        match perso.role with 
          | Archer ->70
          | Magicien-> 50
          | Guerrier-> 30 
    in if chance >100 then 
          100 
        else 
          chance  


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
    if chance < (chance_toucher perso ) then 
      nb_degats perso
    else 
      0 
  

   (**
    Pour savoir si le personnage possède un objet avec un certain quantité
    @auteur
    @param pers le personnage dont on veut savoir s'il possède la quantité et l'objet
    @param obj l'objet à regarder s'il en possède
    @param n la quantité minimum de l'objet requis
    @return true si il a l'objet avec la quantité nécessaire dans son sac false sinon
  *)
    let avoir_objet : perso -> Objet.type_obj-> int -> bool = fun pers obj n->
      let rec aux = fun sac -> 
        match sac with
          | [] -> false
          | {type_obj=a; qte=b}::_ when a=obj && b>=n -> true
          | h::t ->aux t
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
          | {type_obj=a; qte=b}::t when a=t_obj && b<(-n) -> raise (Objet_insuffisant a)
          | {type_obj=a; qte=b}::t when a=t_obj && b>(-n) -> 
              let le_sac = (nouveauSac @ ({type_obj=a; qte=b+n}:: t) ) in
                {nom = perso.nom; sexe = perso.sexe; role = perso.role; pv = perso.pv; xp = perso.xp; niveau = perso.niveau; sac = le_sac }
          | h::t -> aux (nouveauSac @ [h]) t
      in aux [] (perso.sac)
  

  (**
    Le personnage mange un poulet de son sac et obtient 2 points de vie supplémentaires
    Si le personnage ne possède pas de poulet, il ne peut pas manger
    @auteur
    @param perso le personnage qui veut manger
    @return (true et le personnage enlevé d'un poulet dans son sac ) ou ( false et le personnage initial )
  *)
  let manger : perso -> (bool * perso) = fun perso -> 
    if (not(avoir_objet  perso Objet.Poulet 1) )  then 
      (false,perso)
    else 
      (true, (modifier_sac (Objet.Poulet) (-1) (mis_a_jour_pv 2. perso)))


  (**
      Pour accorder les verbes, adjectifs selon le genre du personnage
      @auteur
      @param perso le personnage pour avoir son genre
      @param masculin le mot en masculin
      @param feminin le mot en féminin
      @return le mot en accord avec le genre du personnage
  *)
  let accord_masculin_feminin : perso -> string -> string -> string= fun perso masculin feminin ->
    match perso.sexe with
      |Homme -> masculin
      |Femme -> feminin


  (**
    Le personnage dort et gagne 4 points de vie supplémentaires si aucun danger ne se passe durant sa nuit ou sa sieste
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
        ((print_string ("Malheureusement, vous vous faites attaquer dans la nuit.\n" ^ Monstre.nom_monstre lemonstre ^ " vous attaque et vous êtes "^ (accord_masculin_feminin perso "mort.\n" "morte.\n"))); 
        raise (Tue_En_Dormant lemonstre))
      else 
        print_string ("Vous passez une nuit revigorante et êtes " ^( accord_masculin_feminin perso "prêt" "prête" ) ^
        " à reprendre l'aventure... \nOu se rendormir.\n");
        mis_a_jour_pv 4. perso


  (**
    Le changement de niveau du personnage s'effectue si le point d'expérience est supérieur 
    au point d'expérience maximum de son niveau actuel
    S'il atteint le niveau 10 alors le jeu est fini
    @auteur
    @param p le personnage joué
    @param xp le nouveau point d'expérience du personnage 
    @raise LevelMax quand le personnage atteint le niveau 10
    @return le personnage avec peut être une augmentation de niveau ou bien un message indiquant la fin du jeu 
  *)
  let changement_niveau : perso -> int -> perso = fun p xp ->
    let rec aux : int -> int -> perso= fun le_xp le_niveau ->
      let xp_final_du_niveau =int_of_float( (2. ** float(le_niveau) )*. 10. )in 
      if le_niveau = 10 then
        raise LevelMax
      else 
        if le_xp < xp_final_du_niveau then
          let () =
          if le_niveau != p.niveau then print_string ("Félicitations, vous passez au niveau "^(string_of_int le_niveau) ^".\n")
          in
          {nom = p.nom ; sexe = p.sexe; role = p.role; pv = p.pv; xp = le_xp; niveau =le_niveau ; sac = p.sac }
        else
          let nouv_xp = le_xp - xp_final_du_niveau in 
          let nouv_niveau = le_niveau +1 in 
          aux nouv_xp nouv_niveau
    in aux xp p.niveau

  
  (**
     Le message quand le personnage frappe 
    un message s'il manque sa cible
    et un autre s'il arrive à l'avoir
    @auteur
    @param p le personnage qui frappe
    @param frappe la frappe du personnage, si c'est 0 alors il a manqué sa cible sinon il l'a eu
  *)
  let message_attaque :perso -> int -> string = fun p frappe ->    
    match frappe with 
      | 0 -> "Vous portez une attaque, mais vous manquez votre cible. \n"
      | _ -> "Vous frappez et infligez "^  (string_of_int(nb_degats p)) ^ " points de dégât. \n"


end;;

