
type npocc = {
  n : string;
  p : string;
  occ : Int32.t;
}

type index = Int32.t

type requested_fields = {
  index : bool;
  npocc : bool;
}

type person_select = Index of index | Npocc of npocc

type confidentiality = {
  visible : bool;
  hidden_names : bool;
}

type person = {
  base : Gwdb.base;
  confidentiality : confidentiality;
  fields : requested_fields;
  person : Gwdb.person;
}

type family

module Person : sig
  type t = person
  val get_person : Geneweb.Config.config -> Gwdb.base -> person_select -> requested_fields -> t option
  val get_index : t -> index option
  val get_npocc : t -> npocc option
end = struct

  type t = person

  let of_person conf base fields person =
    let visible = Geneweb.Person.is_visible conf base person in
    let hidden_names = Geneweb.Util.is_hide_names conf person in
    let confidentiality = {visible; hidden_names} in
    {base; fields; confidentiality; person}

  let of_field field f p =
    if field then Some (f p.person) else None

  let get_index person =
    of_field person.fields.index (fun person ->
        Int32.of_int (Gwdb.int_of_iper (Gwdb.get_iper person))
      ) person

  let get_npocc person =
    if not person.confidentiality.visible && person.confidentiality.hidden_names then None
    else
      of_field person.fields.npocc (fun person' ->
          let string istr = Name.lower (Gwdb.sou person.base istr) in
          {
            n = string (Gwdb.get_first_name person');
            p = string (Gwdb.get_surname person');
            occ = Int32.of_int (Gwdb.get_occ person');
          }
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
    with _ -> None
end


module Family : sig
  type t = family
end = struct
  type t = family
end
