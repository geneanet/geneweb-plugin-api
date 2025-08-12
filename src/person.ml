
type confidentiality = {
  visible : bool;
  hidden_names : bool;
}

type t = {
  base : Gwdb.base;
  confidentiality : confidentiality;
  fields : Common.requested_fields;
  person : Gwdb.person;
}

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
          Common.n = string (Gwdb.get_first_name person');
          p = string (Gwdb.get_surname person');
          occ = Int32.of_int (Gwdb.get_occ person');
        }
      ) person

let index_of_npocc base {Common.n; p; occ} =
  Gwdb.person_of_key base p n (Int32.to_int occ)

let index_of_select base = function
  | Common.Index index -> Some (Gwdb.iper_of_int (Int32.to_int index))
  | Npocc npocc -> index_of_npocc base npocc

let get_person conf base select fields =
  try
    Option.map (fun iper ->
        of_person conf base fields (Gwdb.poi base iper)
      ) (index_of_select base select)
  with _ -> None
