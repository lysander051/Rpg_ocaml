open GestionAventure;;

print_string "\n\n\n\nBonjour jeune aventurier, es-tu prêt à mour... gagner. 
Pour ce faire ton but est d'être la personne la plus préstigieuse au monde.

Au faite qui es-tu aventurier?\n\n\n";;

let personnage = GestionAventure.init_aventure();;
GestionAventure.hubAventure personnage;;
