type npocc = {
  n : string;
  p : string;
  occ : Int32.t;
}

type index = Int32.t

type sex = Unknown | Male | Female

type basic_infos = {
  lastname : string option;
  firstname : string option;
  sex : sex option;
  image : string option;
  public_name : string option;
  aliases : string list option;
  qualifiers : string list option;
  firstname_aliases : string list option;
  surname_aliases : string list option;
}

type requested_fields = {
  index : bool;
  npocc : bool;
  basic_infos : bool;
  parents : bool
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

let as_some_string ?(none_if_empty = false) proj person =
  let s = as_string proj person in
  Ext_option.return_if (not none_if_empty || s <> "") (fun () -> s)

let as_some_strings proj person =
  Some (as_strings proj person)

type family

module Person : sig
  type t = person
  val get_person : Geneweb.Config.config -> Gwdb.base -> person_select -> requested_fields -> t option
  val get_index : t -> index option
  val get_npocc : t -> npocc option
  val get_basic_infos : t -> basic_infos option
  val get_father : t -> t option
  val get_mother : t -> t option
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

  let empty_basic_infos = {
    lastname = None;
    firstname = None;
    sex = None;
    image = None;
    public_name = None;
    aliases = None;
    qualifiers = None;
    firstname_aliases = None;
    surname_aliases = None;
  }

  let get_basic_infos person =
    of_field person.fields.basic_infos (fun _person' ->
        let sex = Some (get_sex person) in
        if not (has_visible_names person) then {empty_basic_infos with sex}
        else {
          lastname = as_some_string Gwdb.get_surname person;
          firstname = as_some_string Gwdb.get_first_name person;
          sex;
          image = as_some_string ~none_if_empty:true Gwdb.get_image person;
          public_name = as_some_string ~none_if_empty:true Gwdb.get_public_name person;
          aliases = as_some_strings Gwdb.get_aliases person;
          qualifiers = as_some_strings Gwdb.get_qualifiers person;
          firstname_aliases = as_some_strings Gwdb.get_first_names_aliases person;
          surname_aliases = as_some_strings Gwdb.get_surnames_aliases person;
        }
      ) person

  let parent_fields = {index = true; npocc = true; basic_infos = true; parents = false}

  let get_parent proj person =
    let ( >>= ) = Option.bind in
    if person.fields.parents then
      Gwdb.get_parents person.person >>= fun parents ->
      Option.some (Gwdb.foi person.base parents) >>= fun family ->
      Option.some (proj family) >>= fun iparent ->
      Option.some (of_person person.conf person.base parent_fields (Gwdb.poi person.base iparent))
    else None

  let get_father = get_parent Gwdb.get_father

  let get_mother = get_parent Gwdb.get_mother

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
