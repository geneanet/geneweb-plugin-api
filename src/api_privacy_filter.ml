type t = {
  is_hidden : bool;
  is_visible : bool;
}

let evaluate conf base p =
  let is_visible = Geneweb.Person.is_visible conf base p in
  let is_hidden = Geneweb.Person.is_hide_names conf p && not is_visible in
  { is_hidden; is_visible }

let is_hidden t = t.is_hidden
let is_visible t = t.is_visible

let surname_lower t base p =
  if t.is_hidden then ""
  else Name.lower (Gwdb.sou base (Gwdb.get_surname p))

let first_name_lower t base p =
  if t.is_hidden then ""
  else Name.lower (Gwdb.sou base (Gwdb.get_first_name p))

let surname ?(mask = "x") t gen_p =
  if t.is_hidden then mask else gen_p.Def.surname

let first_name ?(mask = "x") t gen_p =
  if t.is_hidden then mask else gen_p.Def.first_name

let public_name t gen_p =
  let publicname =
    if t.is_hidden then "" else gen_p.Def.public_name
  in
  if publicname = "" then None else Some publicname

let aliases t gen_p =
  if t.is_hidden then [] else gen_p.Def.aliases

let qualifiers t gen_p =
  if t.is_hidden then [] else gen_p.Def.qualifiers

let first_names_aliases t gen_p =
  if t.is_hidden then [] else gen_p.Def.first_names_aliases

let surnames_aliases t gen_p =
  if t.is_hidden then [] else gen_p.Def.surnames_aliases

let titles t conf base p =
  if not t.is_visible then []
  else
    List.map
      (fun x ->
        Adef.as_string
          (Geneweb.Perso.string_of_title ~safe:true ~link:false conf base
             (Adef.safe "") p x))
      (Geneweb.Perso.nobility_titles_list conf base p)

let titles_with_links t conf base p =
  if not t.is_visible then []
  else
    List.map
      (fun x ->
        Adef.as_string
          (Geneweb.Perso.string_of_title ~link:true conf base (Adef.safe "") p x))
      (Geneweb.Perso.nobility_titles_list conf base p)
