open Personnage;;

let read = fun s ->
  let () = print_string s in
  read_line();;
let init_aventure = fun ()->
  let n = read("Ton nom: ") in
  let s = read("Ton genre: ") in
  let c = read("Ta classe: ") in
  print_string (n ^ s ^c);;
;;


