type npocc = {
  n : string;
  p : string;
  occ : Int32.t;
}

type index = Int32.t

type sex = Unknown | Male | Female

type name_aliases = {
  aliases : string list option;
  qualifiers : string list option;
  firstname_aliases : string list option;
  surname_aliases : string list option;
}

type requested_fields = {
  index : bool;
  npocc : bool;
  lastname : bool;
  firstname : bool;
  sex : bool;
  image : bool;
  public_name : bool;
  name_aliases : bool;
  father : requested_fields option;
  mother : requested_fields option;
  sosa : bool;
}

type person_select = Index of index | Npocc of npocc

type confidentiality = {
  visible : bool;
  hidden_names : bool;
}

type person = {
  conf : Geneweb.Config.config;
  base : Gwdb.base;
  confidentiality : confidentiality;
  fields : requested_fields;
  person : Gwdb.person;
}

let has_visible_names person =
  person.confidentiality.visible || not person.confidentiality.hidden_names

let as_string proj person =
  Gwdb.sou person.base (proj person.person)

let as_strings proj person =
  List.map (Gwdb.sou person.base) (proj person.person)

(*let as_some_string ?(none_if_empty = false) proj person =
  let s = as_string proj person in
  Ext_option.return_if (not none_if_empty || s <> "") (fun () -> s)*)

let as_some_strings proj person =
  Some (as_strings proj person)

type family

module Person : sig
  type t = person
  val get_person : Geneweb.Config.config -> Gwdb.base -> person_select -> requested_fields -> t option
  val get_index : t -> index option
  val get_npocc : t -> npocc option
  val get_sex : t -> sex option
  val get_lastname : t -> string option
  val get_firstname : t -> string option
  val get_public_name: t -> string option
  val get_image : t -> string option
  val get_name_aliases : t -> name_aliases option
  val get_father : t -> t option
  val get_mother : t -> t option
  val get_sosa : t -> string option
end = struct

  type t = person

  let of_person conf base fields person =
    let visible = Geneweb.Person.is_visible conf base person in
    let hidden_names = Geneweb.Util.is_hide_names conf person in
    let confidentiality = {visible; hidden_names} in
    {conf; base; fields; confidentiality; person}

  let of_field field f p =
    Ext_option.return_if field (fun () -> f p.person)

  let get_index person =
    of_field person.fields.index (fun person ->
        Int32.of_int (Gwdb.int_of_iper (Gwdb.get_iper person))
      ) person

  let get_npocc person =
    if not (has_visible_names person) then None
    else
      of_field person.fields.npocc (fun person' ->
          let lowered_string proj person = Name.lower (as_string proj person) in
          {
            n = lowered_string Gwdb.get_first_name person;
            p = lowered_string Gwdb.get_surname person;
            occ = Int32.of_int (Gwdb.get_occ person');
          }
        ) person

  let get_sex person = match Gwdb.get_sex person.person with
    | Def.Neuter -> Unknown
    | Def.Male -> Male
    | Def.Female -> Female
  let get_sex person = of_field person.fields.sex (fun _person' -> get_sex person) person

  let get_lastname person =
    if has_visible_names person then
      of_field person.fields.lastname (fun _person' ->
          as_string Gwdb.get_surname person) person
    else None

  let get_firstname person =
    if has_visible_names person then
      of_field person.fields.firstname (fun _person' ->
          as_string Gwdb.get_first_name person) person
    else None

  let get_image person =
    if person.fields.image then
      Api_util.get_portrait person.conf person.base person.person
    else None

  let get_public_name person =
    if has_visible_names person then
      of_field person.fields.public_name (fun _person' ->
          as_string Gwdb.get_public_name person) person
    else None

  let get_name_aliases person =
    if has_visible_names person then
      of_field person.fields.name_aliases (fun _person' ->
          {
            aliases = as_some_strings Gwdb.get_aliases person;
            qualifiers = as_some_strings Gwdb.get_qualifiers person;
            firstname_aliases = as_some_strings Gwdb.get_first_names_aliases person;
            surname_aliases = as_some_strings Gwdb.get_surnames_aliases person;
          }
        ) person
    else None

  let get_parent fields proj person =
    Option.bind fields (fun fields ->
        Option.bind (Gwdb.get_parents person.person) (fun parents ->
        let iparent = proj (Gwdb.foi person.base parents) in
        let parent = Gwdb.poi person.base iparent in
        Option.some (of_person person.conf person.base fields parent))
      )

  let get_father person = get_parent person.fields.father Gwdb.get_father person

  let get_mother person = get_parent person.fields.mother Gwdb.get_mother person

  let get_sosa person =
    of_field person.fields.sosa (fun person' ->
        let sosa_nb_num = Geneweb.Sosa_cache.get_sosa_person
            ~conf:person.conf
            ~base:person.base
            ~person:person'
        in
        Sosa.to_string sosa_nb_num
      ) person

  let index_of_npocc base {n; p; occ} =
    Gwdb.person_of_key base p n (Int32.to_int occ)

  let index_of_select base = function
    | Index index -> Some (Gwdb.iper_of_int (Int32.to_int index))
    | Npocc npocc -> index_of_npocc base npocc

  let get_person conf base select fields =
    try
      Option.map (fun iper ->
          of_person conf base fields (Gwdb.poi base iper)
        ) (index_of_select base select)
    with Failure _ -> None
end

module Family : sig
  type t = family
end = struct
  type t = family
end
