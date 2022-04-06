open Personnage;;

let read = fun s ->
  let () = print_string s in
  read_line();;
let init_aventure = fun ()->
  let n = read("Ton nom: ") in
  let s = read("\n\nTon genre: 
                F) Femme
                H) Homme") in
  let c = read("\n\nTa classe: 
                A) Archer
                G) Guerrier
                M) Mage") in
  print_string (n ^ s ^c);;
;;


