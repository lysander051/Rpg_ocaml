module MONSTRE =
struct 

  type type_monstre = Golem | Sanglier | Nuee of int
  type monstre = { creature : type_monstre; loot : objet ; pv : int}
end;;