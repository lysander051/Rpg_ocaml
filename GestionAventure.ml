open Personnage;;

exception Lecture_Creation_Perso;;
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
  print_string (n ^ g ^c)
;;

