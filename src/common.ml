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
  elements_per_page : [ `Int of int | `All ];
  spouse : requested_fields option;
}

and requested_fields = {
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
  birth : bool;
  baptism : bool;
  death : bool;
  burial : bool;
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
type date = Date.date

type event = {
  event_type : event_type;
  date : date option;
  place : string option;
  notes : string option;
  sources : string option;
  spouse : person option;
  death_type : Def.death option;
  burial_type : Def.burial option;
}

type paginated_events = event list paginated

let has_visible_names person =
  person.confidentiality.visible || not person.confidentiality.hidden_names

let as_string proj person =
  Utf8.normalize (Gwdb.sou person.base (proj person.person))

let as_strings proj person =
  List.map (fun istr -> Utf8.normalize (Gwdb.sou person.base istr)) (proj person.person)

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
  val get_birth : t -> event option
  val get_baptism : t -> event option
  val get_death : t -> event option
  val get_burial : t -> event option
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
    Ext_option.return_if (has_visible_names person) (fun () ->
        of_field person.fields.npocc (fun person' ->
            let lowered_string proj person = Name.lower (as_string proj person) in
            {
              n = lowered_string Gwdb.get_first_name person;
              p = lowered_string Gwdb.get_surname person;
              occ = Int32.of_int (Gwdb.get_occ person');
            }
          ) person)
    |> Option.join

  let get_sex person = match Gwdb.get_sex person with
    | Def.Neuter -> Unknown
    | Def.Male -> Male
    | Def.Female -> Female

  let get_sex person =
    Ext_option.return_if (
      not person.confidentiality.restricted &&
      person.fields.sex)
      (fun () -> get_sex person.person)

  let get_lastname person =
    Ext_option.return_if (has_visible_names person && person.fields.lastname) (fun () ->
        as_string Gwdb.get_surname person)

  let get_firstname person =
    Ext_option.return_if (has_visible_names person && person.fields.firstname) (fun () ->
          as_string Gwdb.get_first_name person)

  let get_image person =
    Ext_option.return_if person.fields.image (fun () ->
      Api_util.get_portrait person.conf person.base person.person)
  |> Option.join

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
    Ext_option.return_if (not person.confidentiality.restricted) (fun () ->
        Option.bind fields (fun fields ->
            Option.bind (Gwdb.get_parents person.person) (fun parents ->
                let iparent = proj (Gwdb.foi person.base parents) in
                let parent = Gwdb.poi person.base iparent in
                Option.some (of_person person.conf person.base fields parent))
          ))
    |> Option.join

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

  let event_type base e =
    Geneweb.Event.map_event_name (fun n -> Utf8.normalize (Gwdb.sou base n)) e

  let event_place base e =
    let place = Utf8.normalize (Gwdb.sou base (Geneweb.Event.get_place e)) in
    Ext_option.return_if (place <> "") (fun () ->
        Adef.as_string (Geneweb.Util.string_of_place place))

  let event_notes conf base person evt =
    let notes_istr = Geneweb.Event.get_note evt in
    Ext_option.return_if (not conf.Geneweb.Config.no_note && not (Gwdb.is_empty_string notes_istr)) (fun () ->
        let note_string = Geneweb.Notes.person_note
            ~keep_newlines:true conf base person
            (Gwdb.sou base (notes_istr))
        in
        Utf8.normalize (Adef.as_string note_string))

  let event_sources conf base evt =
    let src_istr = Geneweb.Event.get_src evt in
    Ext_option.return_if (not @@ Gwdb.is_empty_string src_istr) (fun () ->
        let source_string = Geneweb.Notes.source conf base (Gwdb.sou base src_istr) in
        Utf8.normalize (Adef.as_string source_string))

  let event_spouse conf base spouse_fields evt =
    Option.bind spouse_fields (fun fields ->
        let spouse_iper = Geneweb.Event.get_spouse_iper evt in
        let spouse = Option.map (Gwdb.poi base) spouse_iper in
        Option.map (of_person conf base fields) spouse
      )

  let of_event person spouse_fields evt =
    {
      event_type = event_type person.base (Geneweb.Event.get_name evt);
      date = Date.od_of_cdate (Geneweb.Event.get_date evt);
      place = event_place person.base evt;
      notes = event_notes person.conf person.base person.person evt;
      sources = event_sources person.conf person.base evt;
      spouse = event_spouse person.conf person.base spouse_fields evt;
      death_type = None;
      burial_type = None;
    }

  let get_events person =
    Ext_option.return_if (
      person.confidentiality.visible &&
      Option.is_some person.fields.events) (fun () ->
        let events_request : event_request = Option.get person.fields.events in
        let events = Geneweb.Event.sorted_events person.conf person.base person.person in
        let paginated_events = match events_request.elements_per_page with
          | `All -> Api_util.Paginated_data.all events
          | `Int element_count ->
            let page = Api_util.Page.make ~number:events_request.page_number ~element_count in
            Api_util.Paginated_data.extract page events
        in
        let paginated_events = Api_util.Paginated_data.map (fun evt ->
            of_event person events_request.spouse evt
          ) paginated_events
        in
        {
          elements = paginated_events.Api_util.Paginated_data.elements;
          page_number = paginated_events.page_number;
          total_count = paginated_events.total_count;
        }
      )

  let pers_event ~name ~date ~place ~note ~source ~witnesses =
    Geneweb.Event.event_item_of_gen_pevent
      {
        Def.epers_name = name;
        epers_date = date;
        epers_place = place;
        epers_note = note;
        epers_src = source;
        epers_reason = Gwdb.empty_string;
        epers_witnesses = witnesses;
      }

  let get_main_event ~person ~field ~name ~get_date ~get_place ~get_note ~get_source =
    let evt = Ext_option.return_if (person.confidentiality.visible && field) (fun () ->
        let date = get_date person.person in
        let place = get_place person.person in
        let note = get_note person.person in
        let source = get_source person.person in
        of_event person None (pers_event ~name ~date ~place ~note ~source ~witnesses:[||])
      )
    in
    Option.bind evt (fun evt ->
        Ext_option.return_if (
          Option.is_some evt.date ||
          Option.is_some evt.place ||
          Option.is_some evt.notes ||
          Option.is_some evt.sources)
          (fun () -> evt)
      )

  let get_birth person =
    get_main_event
      ~person
      ~field:person.fields.birth
      ~name:Def.Epers_Birth
      ~get_date:Gwdb.get_birth
      ~get_place:Gwdb.get_birth_place
      ~get_note:Gwdb.get_birth_note
      ~get_source:Gwdb.get_birth_src

  let get_baptism person =
    get_main_event
      ~person
      ~field:person.fields.baptism
      ~name:Def.Epers_Baptism
      ~get_date:Gwdb.get_baptism
      ~get_place:Gwdb.get_baptism_place
      ~get_note:Gwdb.get_baptism_note
      ~get_source:Gwdb.get_baptism_src

  let get_death_date person = match Gwdb.get_death person with
    | Def.Death (_, date) -> date
    | NotDead
    | DeadYoung
    | DeadDontKnowWhen
    | DontKnowIfDead
    | OfCourseDead -> Date.cdate_None

  let get_burial_date person = match Gwdb.get_burial person with
    | Def.Buried date
    | Cremated date -> date
    | UnknownBurial -> Date.cdate_None

  let get_death person =
    let evt = get_main_event
        ~person
        ~field:person.fields.death
        ~name:Def.Epers_Death
        ~get_date:get_death_date
        ~get_place:Gwdb.get_death_place
        ~get_note:Gwdb.get_death_note
        ~get_source:Gwdb.get_death_src
    in
    Option.map (fun evt ->
        {evt with death_type = Some (Gwdb.get_death person.person)}
      ) evt

  let get_burial person =
    let evt = get_main_event
      ~person
      ~field:person.fields.burial
      ~name:Def.Epers_Burial
      ~get_date:get_burial_date
      ~get_place:Gwdb.get_burial_place
      ~get_note:Gwdb.get_burial_note
      ~get_source:Gwdb.get_burial_src
    in
    Option.map (fun evt ->
        {evt with burial_type = Some (Gwdb.get_burial person.person)}
      ) evt

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
