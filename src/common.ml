type npocc = { n : string; p : string; occ : Int32.t }
type index = Int32.t
type sex = Unknown | Male | Female

type name_aliases = {
  aliases : string list option;
  qualifiers : string list option;
  firstname_aliases : string list option;
  surname_aliases : string list option;
}

type 'a paginated_request = {
  page_number : int;
  elements_per_page : [ `Int of int | `All ];
  elements_request : 'a;
}

type marriage_type =
  [ `married
  | `not_married
  | `engaged
  | `no_sexes_check_not_married
  | `no_mention
  | `no_sexes_check_married
  | `marriage_bann
  | `marriage_contract
  | `marriage_license
  | `pacs
  | `residence ]

type divorce_type = [ `not_divorced | `divorced | `separated ]

type visibility =
  [ `visibility_public | `visibility_semi_public | `visibility_private ]

type event_request = {
  spouse : requested_fields option;
  witnesses : paginated_witness_request option;
  event_notes_raw : bool;
  date_str : bool;
}

and witness_request = { witness : requested_fields option; note : bool }
and paginated_witness_request = witness_request paginated_request
and paginated_event_request = event_request paginated_request

and family_request = {
  fam_spouse : requested_fields option;
  marriage_date : bool;
  marriage_place : bool;
  marriage_note : bool;
  marriage_note_raw : bool;
  marriage_source : bool;
  marriage_type : bool;
  divorce_type : bool;
  divorce_date : bool;
  children : requested_fields option;
  fam_witnesses : paginated_witness_request option;
  marriage_date_str : bool;
  divorce_date_str : bool;
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
  notes_raw : bool;
  sources : bool;
  titles : bool;
  events : paginated_event_request option;
  birth : bool;
  baptism : bool;
  death : bool;
  burial : bool;
  related : requested_fields option;
  rparents : requested_fields option;
  families : family_request option;
  visibility : bool;
  is_contemporary : bool;
}

type person_select = Index of index | Npocc of npocc

type person = {
  conf : Geneweb.Config.config;
  base : Gwdb.base;
  restricted : bool;
  privacy : Api_privacy_filter.t;
  fields : requested_fields;
  person : Gwdb.person;
}

type 'a paginated = { elements : 'a list; page_number : int; total_count : int }
type event_type = string Geneweb.Event.event_name
type date = Date.date

type witness = {
  witness_type : Def.witness_kind;
  witness : person;
  note : string option;
}

type paginated_witnesses = witness paginated

type event = {
  event_type : event_type;
  date : date option;
  date_str : string option;
  date_long : string option;
  date_conv : string option;
  date_conv_long : string option;
  date_raw : string option;
  place : string option;
  notes : string option;
  notes_raw : string option;
  sources : string option;
  spouse : person option;
  death_type : Def.death option;
  burial_type : Def.burial option;
  witnesses : paginated_witnesses option;
}

type paginated_events = event paginated

type family = {
  index : int;
  fam_spouse : person option;
  marriage_date : date option;
  marriage_date_str : string option;
  marriage_date_long : string option;
  marriage_date_conv : string option;
  marriage_date_conv_long : string option;
  marriage_date_raw : string option;
  marriage_place : string option;
  marriage_note : string option;
  marriage_note_raw : string option;
  marriage_source : string option;
  marriage_type : marriage_type option;
  divorce_type : divorce_type option;
  divorce_date : date option;
  divorce_date_str : string option;
  divorce_date_long : string option;
  divorce_date_conv : string option;
  divorce_date_conv_long : string option;
  divorce_date_raw : string option;
  children : person list option;
  fam_witnesses : witness list option;
}

let has_visible_names person = not (Api_privacy_filter.is_hidden person.privacy)

let as_string proj person =
  Utf8.normalize (Gwdb.sou person.base (proj person.person))

let as_strings proj person =
  List.map
    (fun istr -> Utf8.normalize (Gwdb.sou person.base istr))
    (proj person.person)

let as_some_strings proj person = Some (as_strings proj person)

let format_date_strings conf req_date_str od =
  match (req_date_str, od) with
  | true, Some d ->
      let d_str, d_long, d_conv, d_conv_long, _cal =
        Api_util.string_of_date_and_conv conf d
      in
      let d_raw = Api_util.string_of_date_raw conf d in
      (Some d_str, Some d_long, Some d_conv, Some d_conv_long, Some d_raw)
  | _ -> (None, None, None, None, None)

module Person : sig
  type t = person

  val get_person :
    Geneweb.Config.config ->
    Gwdb.base ->
    person_select ->
    requested_fields ->
    t option

  val get_index : t -> index option
  val get_npocc : t -> npocc option
  val get_sex : t -> sex option
  val get_lastname : t -> string option
  val get_firstname : t -> string option
  val get_public_name : t -> string option
  val get_image : t -> string option
  val get_name_aliases : t -> name_aliases option
  val get_father : t -> t option
  val get_mother : t -> t option
  val get_sosa : t -> string option
  val get_occupation : t -> string option
  val get_notes : t -> string option
  val get_notes_raw : t -> string option
  val get_sources : t -> string option
  val get_titles : t -> string list option
  val get_events : t -> paginated_events option
  val get_birth : t -> event option
  val get_baptism : t -> event option
  val get_death : t -> event option
  val get_burial : t -> event option
  val get_related : t -> (Def.relation_type * person) list option
  val get_rparents : t -> (Def.relation_type * person) list option
  val get_families : t -> family list option
  val get_visibility : t -> visibility option
  val get_is_contemporary : t -> bool option
  val has_parent : t -> bool
  val has_spouse : t -> bool
  val has_child : t -> bool
end = struct
  type t = person

  let of_person conf base fields person =
    let restricted =
      Geneweb.Person.is_restricted conf base (Gwdb.get_iper person)
    in
    let privacy = Api_privacy_filter.evaluate conf base person in
    { conf; base; restricted; privacy; fields; person }

  let of_field field f p = Ext_option.return_if field (fun () -> f p.person)

  let get_index person =
    Ext_option.return_if
      (has_visible_names person && person.fields.index)
      (fun () -> Int32.of_int (Gwdb.int_of_iper (Gwdb.get_iper person.person)))

  let get_npocc person =
    Ext_option.return_if (has_visible_names person) (fun () ->
        of_field person.fields.npocc
          (fun person' ->
            let lowered_string proj person =
              Name.lower (as_string proj person)
            in
            {
              n = lowered_string Gwdb.get_first_name person;
              p = lowered_string Gwdb.get_surname person;
              occ = Int32.of_int (Gwdb.get_occ person');
            })
          person)
    |> Option.join

  let get_sex person =
    match Gwdb.get_sex person with
    | Def.Neuter -> Unknown
    | Def.Male -> Male
    | Def.Female -> Female

  let get_sex person =
    Ext_option.return_if ((not person.restricted) && person.fields.sex)
      (fun () -> get_sex person.person)

  let get_lastname person =
    Ext_option.return_if
      (has_visible_names person && person.fields.lastname)
      (fun () -> as_string Gwdb.get_surname person)

  let get_firstname person =
    Ext_option.return_if
      (has_visible_names person && person.fields.firstname)
      (fun () -> as_string Gwdb.get_first_name person)

  let get_image person =
    Ext_option.return_if person.fields.image (fun () ->
        Api_util.get_portrait person.conf person.base person.person)
    |> Option.join

  let get_public_name person =
    Ext_option.return_if
      (has_visible_names person && person.fields.public_name)
      (fun () -> as_string Gwdb.get_public_name person)

  let get_name_aliases person =
    Ext_option.return_if
      (has_visible_names person && person.fields.name_aliases)
      (fun () ->
        {
          aliases = as_some_strings Gwdb.get_aliases person;
          qualifiers = as_some_strings Gwdb.get_qualifiers person;
          firstname_aliases =
            as_some_strings Gwdb.get_first_names_aliases person;
          surname_aliases = as_some_strings Gwdb.get_surnames_aliases person;
        })

  let get_parent fields proj person =
    Ext_option.return_if (not person.restricted) (fun () ->
        Option.bind fields (fun fields ->
            Option.bind (Gwdb.get_parents person.person) (fun parents ->
                let iparent = proj (Gwdb.foi person.base parents) in
                if iparent = Gwdb.dummy_iper then None
                else
                  let parent = Gwdb.poi person.base iparent in
                  Some (of_person person.conf person.base fields parent))))
    |> Option.join

  let get_father person = get_parent person.fields.father Gwdb.get_father person
  let get_mother person = get_parent person.fields.mother Gwdb.get_mother person

  let get_sosa person =
    of_field person.fields.sosa
      (fun person' ->
        let sosa_nb_num =
          Geneweb.Sosa_cache.get_sosa_person ~conf:person.conf ~base:person.base
            ~person:person'
        in
        Sosa.to_string sosa_nb_num)
      person

  let get_occupation person =
    Ext_option.return_if
      (Api_privacy_filter.is_visible person.privacy && person.fields.occupation)
      (fun () ->
        Adef.as_string
        @@ Geneweb.Notes.source person.conf person.base
             (as_string Gwdb.get_occupation person))

  let get_notes person =
    Ext_option.return_if
      (Api_privacy_filter.is_visible person.privacy
      && person.fields.notes
      && not person.conf.Geneweb.Config.no_note)
      (fun () ->
        Adef.as_string
          (Geneweb.Notes.person_note ~keep_newlines:true person.conf person.base
             person.person
             (as_string Gwdb.get_notes person)))

  let get_notes_raw person =
    Ext_option.return_if
      (Api_privacy_filter.is_visible person.privacy
      && person.fields.notes_raw
      && not person.conf.Geneweb.Config.no_note)
      (fun () -> as_string Gwdb.get_notes person)

  let get_visibility person =
    Ext_option.return_if person.fields.visibility (fun () ->
        Api_util.get_visibility person.conf person.base person.person)

  let get_is_contemporary person =
    Ext_option.return_if person.fields.is_contemporary (fun () ->
        Geneweb.Person.is_contemporary person.conf person.base person.person)

  let get_sources person =
    Ext_option.return_if
      (Api_privacy_filter.is_visible person.privacy && person.fields.sources)
      (fun () ->
        Adef.as_string
          (Geneweb.Notes.source person.conf person.base
             (as_string Gwdb.get_psources person)))

  let get_titles person =
    Ext_option.return_if person.fields.titles (fun () ->
        Api_privacy_filter.titles person.privacy person.conf person.base
          person.person)

  let event_type base e =
    let map_name n = Utf8.normalize (Gwdb.sou base n) in
    match e with
    | Geneweb.Event.Pevent (Def.Epers_Name n) ->
        Geneweb.Event.Pevent (Def.Epers_Name (map_name n))
    | Geneweb.Event.Fevent (Def.Efam_Name n) ->
        Geneweb.Event.Fevent (Def.Efam_Name (map_name n))
    | Geneweb.Event.Pevent
        (( Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial
         | Epers_Cremation | Epers_Accomplishment | Epers_Acquisition
         | Epers_Adhesion | Epers_BaptismLDS | Epers_BarMitzvah
         | Epers_BatMitzvah | Epers_Benediction | Epers_ChangeName
         | Epers_Circumcision | Epers_Confirmation | Epers_ConfirmationLDS
         | Epers_Decoration | Epers_DemobilisationMilitaire | Epers_Diploma
         | Epers_Distinction | Epers_Dotation | Epers_DotationLDS
         | Epers_Education | Epers_Election | Epers_Emigration
         | Epers_Excommunication | Epers_FamilyLinkLDS | Epers_FirstCommunion
         | Epers_Funeral | Epers_Graduate | Epers_Hospitalisation
         | Epers_Illness | Epers_Immigration | Epers_ListePassenger
         | Epers_MilitaryDistinction | Epers_MilitaryPromotion
         | Epers_MilitaryService | Epers_MobilisationMilitaire
         | Epers_Naturalisation | Epers_Occupation | Epers_Ordination
         | Epers_Property | Epers_Recensement | Epers_Residence | Epers_Retired
         | Epers_ScellentChildLDS | Epers_ScellentParentLDS
         | Epers_ScellentSpouseLDS | Epers_VenteBien | Epers_Will
         | Epers_Adoption ) as pe) ->
        Geneweb.Event.Pevent pe
    | Geneweb.Event.Fevent
        (( Efam_Marriage | Efam_NoMarriage | Efam_NoMention | Efam_Engage
         | Efam_Divorce | Efam_Separated | Efam_Annulation | Efam_MarriageBann
         | Efam_MarriageContract | Efam_MarriageLicense | Efam_PACS
         | Efam_Residence ) as fe) ->
        Geneweb.Event.Fevent fe

  let event_place base e =
    let place = Utf8.normalize (Gwdb.sou base (Geneweb.Event.get_place e)) in
    Ext_option.return_if (place <> "") (fun () ->
        Adef.as_string (Geneweb.Util.string_of_place place))

  let event_notes conf base person evt =
    let notes_istr = Geneweb.Event.get_note evt in
    Ext_option.return_if
      ((not conf.Geneweb.Config.no_note)
      && not (Gwdb.is_empty_string notes_istr))
      (fun () ->
        let note_string =
          Geneweb.Notes.person_note ~keep_newlines:true conf base person
            (Gwdb.sou base notes_istr)
        in
        Utf8.normalize (Adef.as_string note_string))

  let event_sources conf base evt =
    let src_istr = Geneweb.Event.get_src evt in
    Ext_option.return_if
      (not @@ Gwdb.is_empty_string src_istr)
      (fun () ->
        let source_string =
          Geneweb.Notes.source conf base (Gwdb.sou base src_istr)
        in
        Utf8.normalize (Adef.as_string source_string))

  let event_spouse conf base spouse_fields evt =
    Option.bind spouse_fields (fun fields ->
        Option.bind (Geneweb.Event.get_spouse_iper evt) (fun spouse_iper ->
            if spouse_iper = Gwdb.dummy_iper then None
            else Some (of_person conf base fields (Gwdb.poi base spouse_iper))))

  let event_witnesses person evt
      (paginated_witness_request : paginated_witness_request) =
    let witnesses_data = Geneweb.Event.get_witnesses_and_notes evt in
    let witness_fields = paginated_witness_request.elements_request.witness in
    let witnesses =
      match witness_fields with
      | Some witness_fields ->
          Array.fold_left
            (fun acc (iper, witness_type, istr) ->
              let wpers = Gwdb.poi person.base iper in
              let witness =
                of_person person.conf person.base witness_fields wpers
              in
              if Api_privacy_filter.is_visible witness.privacy then
                let note =
                  Ext_option.return_if
                    (not (Gwdb.is_empty_string istr))
                    (fun () -> Utf8.normalize (Gwdb.sou person.base istr))
                in
                { witness; witness_type; note } :: acc
              else acc)
            [] witnesses_data
          |> List.rev
      | None -> []
    in
    let paginated_witnesses =
      match paginated_witness_request.elements_per_page with
      | `All -> Api_util.Paginated_data.all witnesses
      | `Int element_count ->
          let page =
            Api_util.Page.make ~number:paginated_witness_request.page_number
              ~element_count
          in
          Api_util.Paginated_data.extract page witnesses
    in
    {
      elements = paginated_witnesses.Api_util.Paginated_data.elements;
      page_number = paginated_witnesses.page_number;
      total_count = paginated_witnesses.total_count;
    }

  let of_event person spouse_fields
      (paginated_witness_request : paginated_witness_request option) evt =
    let od = Date.od_of_cdate (Geneweb.Event.get_date evt) in
    let date_str, date_long, date_conv, date_conv_long, date_raw =
      format_date_strings person.conf true od
    in
    let req_notes_raw =
      Option.fold ~none:false
        ~some:(fun (r : paginated_event_request) ->
          r.elements_request.event_notes_raw)
        person.fields.events
    in
    let notes_raw =
      Ext_option.return_if
        (req_notes_raw
        && Api_privacy_filter.is_visible person.privacy
        && not person.conf.Geneweb.Config.no_note)
        (fun () ->
          let n = Gwdb.sou person.base (Geneweb.Event.get_note evt) in
          if n = "" then None else Some (Utf8.normalize n))
      |> Option.join
    in
    {
      event_type = event_type person.base (Geneweb.Event.get_name evt);
      date = od;
      date_str;
      date_long;
      date_conv;
      date_conv_long;
      date_raw;
      place = event_place person.base evt;
      notes = event_notes person.conf person.base person.person evt;
      notes_raw;
      sources = event_sources person.conf person.base evt;
      spouse = event_spouse person.conf person.base spouse_fields evt;
      death_type = None;
      burial_type = None;
      witnesses =
        Option.map (event_witnesses person evt) paginated_witness_request;
    }

  let get_events person =
    Ext_option.return_if (Api_privacy_filter.is_visible person.privacy)
      (fun () ->
        Option.map
          (fun (paginated_events_request : paginated_event_request) ->
            let conf = Geneweb.Config.Trimmed.from_config person.conf in
            let auth_person =
              Geneweb.Authorized.Person.make ~conf ~base:person.base
                (Gwdb.get_iper person.person)
            in
            let events =
              Geneweb.Event.sorted_events conf person.base auth_person
            in
            let paginated_events =
              match paginated_events_request.elements_per_page with
              | `All -> Api_util.Paginated_data.all events
              | `Int element_count ->
                  let page =
                    Api_util.Page.make
                      ~number:paginated_events_request.page_number
                      ~element_count
                  in
                  Api_util.Paginated_data.extract page events
            in
            let paginated_events =
              Api_util.Paginated_data.map
                (fun evt ->
                  of_event person
                    paginated_events_request.elements_request.spouse
                    paginated_events_request.elements_request.witnesses evt)
                paginated_events
            in
            {
              elements = paginated_events.Api_util.Paginated_data.elements;
              page_number = paginated_events.page_number;
              total_count = paginated_events.total_count;
            })
          person.fields.events)
    |> Option.join

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

  let get_main_event ~person ~field ~name ~get_date ~get_place ~get_note
      ~get_source =
    let evt =
      Ext_option.return_if
        (Api_privacy_filter.is_visible person.privacy && field)
        (fun () ->
          let date = get_date person.person in
          let place = get_place person.person in
          let note = get_note person.person in
          let source = get_source person.person in
          of_event person None None
            (pers_event ~name ~date ~place ~note ~source ~witnesses:[||]))
    in
    Option.bind evt (fun evt ->
        Ext_option.return_if
          (Option.is_some evt.date || Option.is_some evt.place
         || Option.is_some evt.notes || Option.is_some evt.sources)
          (fun () -> evt))

  let get_birth person =
    get_main_event ~person ~field:person.fields.birth ~name:Def.Epers_Birth
      ~get_date:Gwdb.get_birth ~get_place:Gwdb.get_birth_place
      ~get_note:Gwdb.get_birth_note ~get_source:Gwdb.get_birth_src

  let get_baptism person =
    get_main_event ~person ~field:person.fields.baptism ~name:Def.Epers_Baptism
      ~get_date:Gwdb.get_baptism ~get_place:Gwdb.get_baptism_place
      ~get_note:Gwdb.get_baptism_note ~get_source:Gwdb.get_baptism_src

  let get_death_date person =
    match Gwdb.get_death person with
    | Def.Death (_, date) -> date
    | NotDead | DeadYoung | DeadDontKnowWhen | DontKnowIfDead | OfCourseDead ->
        Date.cdate_None

  let get_burial_date person =
    match Gwdb.get_burial person with
    | Def.Buried date | Cremated date -> date
    | UnknownBurial -> Date.cdate_None

  let get_death person =
    let evt =
      get_main_event ~person ~field:person.fields.death ~name:Def.Epers_Death
        ~get_date:get_death_date ~get_place:Gwdb.get_death_place
        ~get_note:Gwdb.get_death_note ~get_source:Gwdb.get_death_src
    in
    Option.map
      (fun evt -> { evt with death_type = Some (Gwdb.get_death person.person) })
      evt

  let get_burial person =
    let evt =
      get_main_event ~person ~field:person.fields.burial ~name:Def.Epers_Burial
        ~get_date:get_burial_date ~get_place:Gwdb.get_burial_place
        ~get_note:Gwdb.get_burial_note ~get_source:Gwdb.get_burial_src
    in
    Option.map
      (fun evt ->
        { evt with burial_type = Some (Gwdb.get_burial person.person) })
      evt

  let index_of_npocc base { n; p; occ } =
    let occ = Int32.to_int occ in
    match Gwdb.person_of_key base n p occ with
    | Some ip -> Some ip
    | None ->
        Gwdb.person_of_key base
          (String.capitalize_ascii n)
          (String.capitalize_ascii p)
          occ

  let index_of_select base = function
    | Index index -> Some (Gwdb.iper_of_int (Int32.to_int index))
    | Npocc npocc -> index_of_npocc base npocc

  let get_person conf base select fields =
    try
      Option.bind (index_of_select base select) (fun iper ->
          if iper = Gwdb.dummy_iper then None
          else Some (of_person conf base fields (Gwdb.poi base iper)))
    with Invalid_argument _ | Not_found | Failure _ -> None

  let get_related person =
    Option.map
      (fun fields ->
        List.map
          (fun (person', relation) ->
            ( relation.Def.r_type,
              of_person person.conf person.base fields person' ))
          (Geneweb.Relation.get_others_related person.conf person.base
             person.person))
      person.fields.related

  let get_rparents person =
    Option.map
      (fun fields ->
        List.fold_left
          (fun rparents relation ->
            let r_type = relation.Def.r_type in
            let r_fath =
              Option.map
                (fun ip ->
                  of_person person.conf person.base fields
                    (Gwdb.poi person.base ip))
                relation.Def.r_fath
            in
            let r_moth =
              Option.map
                (fun ip ->
                  of_person person.conf person.base fields
                    (Gwdb.poi person.base ip))
                relation.Def.r_moth
            in
            let rparents =
              Option.fold ~none:rparents
                ~some:(fun r_fath -> (r_type, r_fath) :: rparents)
                r_fath
            in
            Option.fold ~none:rparents
              ~some:(fun r_moth -> (r_type, r_moth) :: rparents)
              r_moth)
          []
          (Gwdb.get_rparents person.person))
      person.fields.rparents

  let marriage_type_of_def = function
    | Def.Married -> `married
    | NotMarried -> `not_married
    | Engaged -> `engaged
    | NoSexesCheckNotMarried -> `no_sexes_check_not_married
    | NoMention -> `no_mention
    | NoSexesCheckMarried -> `no_sexes_check_married
    | MarriageBann -> `marriage_bann
    | MarriageContract -> `marriage_contract
    | MarriageLicense -> `marriage_license
    | Pacs -> `pacs
    | Residence -> `residence

  let of_family person (fam_req : family_request) ifam =
    let base = person.base in
    let conf = person.conf in
    let foi = Gwdb.foi base ifam in
    let spouse_iper = Gutil.spouse (Gwdb.get_iper person.person) foi in
    let spouse_opt =
      if spouse_iper = Gwdb.dummy_iper then None
      else Some (Gwdb.poi base spouse_iper)
    in
    let spouse_privacy =
      Option.map (Api_privacy_filter.evaluate conf base) spouse_opt
    in
    let m_auth =
      Api_privacy_filter.is_visible person.privacy
      && Option.fold ~none:false ~some:Api_privacy_filter.is_visible
           spouse_privacy
    in
    let fam_spouse =
      Option.bind fam_req.fam_spouse (fun s_fields ->
          Option.map (of_person conf base s_fields) spouse_opt)
    in
    let marriage_date =
      Ext_option.return_if (m_auth && fam_req.marriage_date) (fun () ->
          Date.od_of_cdate (Gwdb.get_marriage foi))
      |> Option.join
    in
    let ( marriage_date_str,
          marriage_date_long,
          marriage_date_conv,
          marriage_date_conv_long,
          marriage_date_raw ) =
      format_date_strings conf m_auth marriage_date
    in
    let marriage_place =
      Ext_option.return_if (m_auth && fam_req.marriage_place) (fun () ->
          let pl = Gwdb.sou base (Gwdb.get_marriage_place foi) in
          if pl = "" then None
          else
            Some
              (Utf8.normalize
                 (Adef.as_string (Geneweb.Util.string_of_place pl))))
      |> Option.join
    in
    let marriage_note =
      Ext_option.return_if
        (m_auth && fam_req.marriage_note && not conf.Geneweb.Config.no_note)
        (fun () ->
          let n = Gwdb.sou base (Gwdb.get_marriage_note foi) in
          if n = "" then None
          else
            Some
              (Utf8.normalize
                 (Adef.as_string
                    (Geneweb.Notes.note ~keep_newlines:true conf base [] n))))
      |> Option.join
    in
    let marriage_note_raw =
      Ext_option.return_if
        (m_auth && fam_req.marriage_note_raw && not conf.Geneweb.Config.no_note)
        (fun () ->
          let n = Gwdb.sou base (Gwdb.get_marriage_note foi) in
          if n = "" then None else Some (Utf8.normalize n))
      |> Option.join
    in
    let marriage_source =
      Ext_option.return_if (m_auth && fam_req.marriage_source) (fun () ->
          let s = Gwdb.sou base (Gwdb.get_marriage_src foi) in
          let s = if s = "" then Gwdb.sou base (Gwdb.get_fsources foi) else s in
          if s = "" then None
          else
            Some
              (Utf8.normalize
                 (Adef.as_string (Geneweb.Notes.source conf base s))))
      |> Option.join
    in
    let marriage_type =
      Ext_option.return_if fam_req.marriage_type (fun () ->
          marriage_type_of_def (Gwdb.get_relation foi))
    in
    let divorce_type, divorce_date =
      match Gwdb.get_divorce foi with
      | Def.NotDivorced ->
          ( Ext_option.return_if fam_req.divorce_type (fun () -> `not_divorced),
            None )
      | Def.Divorced cod ->
          let d_type =
            Ext_option.return_if fam_req.divorce_type (fun () -> `divorced)
          in
          let d_date =
            Ext_option.return_if (m_auth && fam_req.divorce_date) (fun () ->
                Date.od_of_cdate cod)
            |> Option.join
          in
          (d_type, d_date)
      | Def.Separated ->
          ( Ext_option.return_if fam_req.divorce_type (fun () -> `separated),
            None )
    in
    let ( divorce_date_str,
          divorce_date_long,
          divorce_date_conv,
          divorce_date_conv_long,
          divorce_date_raw ) =
      format_date_strings conf m_auth divorce_date
    in
    let children =
      Option.map
        (fun ch_fields ->
          Array.fold_right
            (fun ichild acc ->
              if ichild = Gwdb.dummy_iper then acc
              else
                let ch_person = Gwdb.poi base ichild in
                of_person conf base ch_fields ch_person :: acc)
            (Gwdb.get_children foi) [])
        fam_req.children
    in
    let fam_witnesses =
      Option.map
        (fun (w_req : paginated_witness_request) ->
          let w_fields = w_req.elements_request.witness in
          match w_fields with
          | None -> []
          | Some wf ->
              Array.fold_right
                (fun (ip, wkind, wnote) acc ->
                  if ip = Gwdb.dummy_iper then acc
                  else
                    let w_pers = Gwdb.poi base ip in
                    let w = of_person conf base wf w_pers in
                    if Api_privacy_filter.is_visible w.privacy then
                      let note =
                        Ext_option.return_if
                          (w_req.elements_request.note
                          && not (Gwdb.is_empty_string wnote))
                          (fun () -> Utf8.normalize (Gwdb.sou base wnote))
                      in
                      { witness = w; witness_type = wkind; note } :: acc
                    else acc)
                (Geneweb.Perso.get_marriage_witnesses_and_notes foi)
                [])
        fam_req.fam_witnesses
    in
    {
      index = Gwdb.int_of_ifam ifam;
      fam_spouse;
      marriage_date;
      marriage_date_str;
      marriage_date_long;
      marriage_date_conv;
      marriage_date_conv_long;
      marriage_date_raw;
      marriage_place;
      marriage_note;
      marriage_note_raw;
      marriage_source;
      marriage_type;
      divorce_type;
      divorce_date;
      divorce_date_str;
      divorce_date_long;
      divorce_date_conv;
      divorce_date_conv_long;
      divorce_date_raw;
      children;
      fam_witnesses;
    }

  let get_families person =
    Option.map
      (fun fam_req ->
        Array.fold_right
          (fun ifam acc -> of_family person fam_req ifam :: acc)
          (Gwdb.get_family person.person)
          [])
      person.fields.families

  let has_parent person = Gwdb.get_parents person.person <> None
  let has_spouse person = Array.length (Gwdb.get_family person.person) >= 1

  let has_child person =
    Array.exists
      (fun ifam ->
        let fam = Gwdb.foi person.base ifam in
        Array.length (Gwdb.get_children fam) >= 1)
      (Gwdb.get_family person.person)
end
