open GestionAventure;;
open Personnage;;
open Monstre;;

print_string ("\n\n\n\n" ^
GestionAventure.delimiteur()^
"> Bonjour jeune aventurier, es-tu prêt à mour... gagner. 
Pour ce faire ton but est simple. 
Deviens la personne la plus expérimentée et accumule des 
objets hors du commun.

Au fait qui es-tu aventurier?\n");;

let personnage = GestionAventure.init_aventure();;
let raison_fin = ref "";;     

try 
  GestionAventure.hubAventure personnage

with 
  | Personnage.Personnage_mort -> raison_fin := "Vous êtes mort. Vous pouvez rejouer pour espérer faire mieux. \n\n"
  | Personnage.Tue_En_Dormant a -> raison_fin := Monstre.nom_monstre_tueur_nuit a^" vous a tué dans la nuit.\nC'est pour cette raison que personne ne part seul à l'aventure\n\n"
  | Personnage.LevelMax -> raison_fin := "Votre aventure est terminée, vous êtes la personne la plus expérimentée au monde. \n(même les développeurs on dû tricher pour voir ce message)\n\n"
  | GestionAventure.Quitte_le_jeu -> raison_fin := "Votre personnage part à la retraite.\nVous ne mourrez pas aujourd'hui, du moins cette fois-ci\n\n"
;;

print_string "\n\n+---------------------------------Fin de partie----------------------------------+\n";;
print_string "> La partie s'est terminé car: \n";;
print_string !raison_fin;;

