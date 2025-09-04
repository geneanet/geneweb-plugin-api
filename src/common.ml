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

type event_request = {
  page_number : int;
  elements_per_page : int;
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
  occupation : bool;
  notes : bool;
  sources : bool;
  titles : bool;
  events : event_request option;
}

type person_select = Index of index | Npocc of npocc

type confidentiality = {
  restricted : bool;
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

type 'a paginated = {
  elements : 'a;
  page_number : int;
  total_count : int;
}

type event_type = string Geneweb.Event.event_name

type event = {
  event_type : event_type
}

type paginated_events = event list paginated

let has_visible_names person =
  person.confidentiality.visible || not person.confidentiality.hidden_names

let as_string proj person =
  Utf8.normalize (Gwdb.sou person.base (proj person.person))

let as_strings proj person =
  List.map (fun istr -> Utf8.normalize (Gwdb.sou person.base istr)) (proj person.person)

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
  val get_occupation : t -> string option
  val get_notes : t -> string option
  val get_sources : t -> string option
  val get_titles : t -> string list option
  val get_events : t -> paginated_events option
end = struct

  type t = person

  let of_person conf base fields person =
    let restricted = Geneweb.Util.is_restricted conf base (Gwdb.get_iper person) in
    let visible = Geneweb.Person.is_visible conf base person in
    let hidden_names = Geneweb.Util.is_hide_names conf person in
    let confidentiality = {restricted; visible; hidden_names} in
    {conf; base; fields; confidentiality; person}

  let of_field field f p =
    Ext_option.return_if field (fun () -> f p.person)

  let get_index person =
    Ext_option.return_if (has_visible_names person && person.fields.index) (fun () ->
        Int32.of_int (Gwdb.int_of_iper (Gwdb.get_iper person.person))
      )

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

  let get_sex person = match Gwdb.get_sex person with
    | Def.Neuter -> Unknown
    | Def.Male -> Male
    | Def.Female -> Female

  let get_sex person =
    of_field person.fields.sex (fun person' ->
        get_sex person'
      ) person

  let get_lastname person =
    Ext_option.return_if (has_visible_names person && person.fields.lastname) (fun () ->
        as_string Gwdb.get_surname person)

  let get_firstname person =
    Ext_option.return_if (has_visible_names person && person.fields.firstname) (fun () ->
          as_string Gwdb.get_first_name person)

  let get_image person =
    if person.fields.image then
      Api_util.get_portrait person.conf person.base person.person
    else None

  let get_public_name person =
    Ext_option.return_if (has_visible_names person && person.fields.public_name) (fun () ->
        as_string Gwdb.get_public_name person)

  let get_name_aliases person =
    Ext_option.return_if (has_visible_names person && person.fields.name_aliases) (fun () ->
        {
          aliases = as_some_strings Gwdb.get_aliases person;
          qualifiers = as_some_strings Gwdb.get_qualifiers person;
          firstname_aliases = as_some_strings Gwdb.get_first_names_aliases person;
          surname_aliases = as_some_strings Gwdb.get_surnames_aliases person;
        })

  let get_parent fields proj person =
    if person.confidentiality.restricted then None
    else
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

  let get_occupation person =
    Ext_option.return_if
      (person.confidentiality.visible
       && person.fields.occupation
      ) (fun () ->
          Adef.as_string @@
          Geneweb.Notes.source
            person.conf
            person.base
            (as_string Gwdb.get_occupation person)
        )

  let get_notes person =
    Ext_option.return_if (
      person.confidentiality.visible &&
      person.fields.notes &&
      not person.conf.Geneweb.Config.no_note
    ) (fun () ->
        Adef.as_string (
          Geneweb.Notes.person_note
            ~keep_newlines:true
            person.conf
            person.base
            person.person
            (as_string Gwdb.get_notes person)
        ))

  let get_sources person =
    Ext_option.return_if (
      person.confidentiality.visible &&
      person.fields.sources)
      (fun () ->
          Adef.as_string (
            Geneweb.Notes.source
              person.conf
              person.base
              (as_string Gwdb.get_psources person)
          ))

  let get_titles person =
    Ext_option.return_if person.fields.titles (fun () ->
        List.map (fun title ->
            Adef.as_string
              (Geneweb.Perso.string_of_title
                 ~safe:true
                 ~link:false
                 person.conf
                 person.base
                 (Adef.safe "") person.person title)
          ) (Geneweb.Perso.nobility_titles_list person.conf person.base person.person)
      )

  let event_type base e = match e with
    | Geneweb.Event.Pevent (Def.Epers_Name istr) -> Geneweb.Event.Pevent (Def.Epers_Name (Utf8.normalize (Gwdb.sou base istr)))
    | Geneweb.Event.Fevent (Def.Efam_Name istr) -> Geneweb.Event.Fevent (Def.Efam_Name (Utf8.normalize (Gwdb.sou base istr)))
    | _ as e -> (Obj.magic e)

  let get_events person =
    Ext_option.return_if (
      person.confidentiality.visible &&
      Option.is_some person.fields.events) (fun () ->
        let events_request = Option.get person.fields.events in
        let page = Api_util.Page.make ~number:events_request.page_number ~element_count:events_request.elements_per_page in
        let events = Geneweb.Event.sorted_events person.conf person.base person.person in
        let paginated_events = Api_util.Paginated_data.extract page events in
        let paginated_events = Api_util.Paginated_data.map (fun evt ->
            {
              event_type = event_type person.base (Geneweb.Event.get_name evt);
            }
          ) paginated_events in
        {
          elements = paginated_events.Api_util.Paginated_data.elements;
          page_number = paginated_events.page_number;
          total_count = paginated_events.total_count;
        }
      )


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
