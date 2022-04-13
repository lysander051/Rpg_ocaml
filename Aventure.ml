open GestionAventure;;

print_string "\n\n\n\nBonjour jeune aventurier, es-tu prêt à mour... gagner. 
Pour ce faire ton but est d'être la personne la plus préstigieuse au monde.

Au faite qui es-tu aventurier?\n\n\n";;

let personnage = GestionAventure.init_aventure();;

try 
GestionAventure.hubAventure personnage

with 
| Personnage.Personnage.Personnage_mort -> print_string "Vous êtes mort\n\n"
| Personnage.Personnage.Tue_En_Dormant _ -> print_string "Un monstre vous a tué dans la nuit\n\n"
| GestionAventure.Quitte_le_jeu -> print_string "Votre personnage part à la retraite\n\n"
;;