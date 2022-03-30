module MONSTRE =
struct 
  type moustique = int
  type type_monstre = Golem | Sanglier | Nuee of moustique
  type monstre = { creature : type_monstre; objet : PERSONNE.objet }
end;;