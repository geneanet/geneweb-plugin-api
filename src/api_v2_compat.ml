(** Compatibility and response emulation adapter bridging legacy API (v1)
    and API v2 engines. *)

let request_of_fiche_parameters
    (params : Api_saisie_read_piqi.Fiche_parameters.t) : Request.t option =
  let ident = params.Api_saisie_read_piqi.Fiche_parameters.identifier_person in
  let person_select =
    match ident.Api_saisie_read_piqi.Identifier_person.index with
    | Some index -> Some (Common.Index index)
    | None -> (
        match
          ( ident.Api_saisie_read_piqi.Identifier_person.n,
            ident.Api_saisie_read_piqi.Identifier_person.p )
        with
        | Some n, Some p ->
            let occ =
              Option.value ~default:0l
                ident.Api_saisie_read_piqi.Identifier_person.oc
            in
            Some (Common.Npocc { n; p; occ })
        | _ -> None)
  in
  let no_event =
    Option.value ~default:false
      params.Api_saisie_read_piqi.Fiche_parameters.no_event
  in
  let events =
    if no_event then None
    else
      Some
        (Request.full_page
           {
             Common.spouse = Some Request.simple_fields;
             witnesses =
               Some
                 (Request.full_page
                    { Common.witness = Some Request.simple_fields; note = true });
             event_notes_raw = true;
             date_str = true;
           })
  in
  let simple_child_fields =
    { Request.simple_fields with birth = true; death = true }
  in
  let requested_fields =
    {
      Request.full_fields with
      events;
      families =
        Some
          {
            Common.fam_spouse = Some simple_child_fields;
            marriage_date = true;
            marriage_place = true;
            marriage_note = true;
            marriage_note_raw = true;
            marriage_source = true;
            marriage_type = true;
            divorce_type = true;
            divorce_date = true;
            children = Some simple_child_fields;
            fam_witnesses =
              Some
                (Request.full_page
                   { Common.witness = Some Request.simple_fields; note = true });
            marriage_date_str = true;
            divorce_date_str = true;
          };
    }
  in
  Option.map
    (fun person_select -> Request.make ~person_select ~requested_fields)
    person_select

let visibility_to_v1 = function
  | Some `visibility_public -> `visibility_public
  | Some `visibility_semi_public -> `visibility_semi_public
  | _ -> `visibility_private

let sex_to_v1 = function
  | Some Common.Male -> `male
  | Some Common.Female -> `female
  | _ -> `unknown

let sosa_of_response (resp : Response.t) : Api_saisie_read_piqi.sosa =
  match resp.sosa with
  | Some s when s = "1" -> `sosa_ref
  | Some s when s <> "" && s <> "0" -> `sosa
  | _ -> `no_sosa

let simple_person_of_response conf (resp : Response.t) :
    Api_saisie_read_piqi.Simple_person.t =
  let fn = Option.value ~default:"" resp.firstname in
  let sn = Option.value ~default:"" resp.lastname in
  let n = match resp.npocc with Some np -> np.p | None -> Name.lower sn in
  let p = match resp.npocc with Some np -> np.n | None -> Name.lower fn in
  let occ = match resp.npocc with Some np -> np.occ | None -> 0l in
  let sosa_nb =
    match resp.sosa with Some s when s <> "" && s <> "0" -> Some s | _ -> None
  in
  let visible_for_visitors = visibility_to_v1 resp.visibility in
  let is_hidden = visible_for_visitors = `visibility_private in
  let birth_short_date, birth_date_raw, birth_place =
    match resp.birth with
    | Some b -> (b.date_str, b.date_raw, b.place)
    | None -> (None, None, None)
  in
  let death_short_date, death_date_raw, death_place =
    match resp.death with
    | Some d -> (d.date_str, d.date_raw, d.place)
    | None -> (None, None, None)
  in
  let has_parent = resp.has_parent in
  let has_spouse = resp.has_spouse in
  let has_child = resp.has_child in
  {
    Api_saisie_read_piqi.Simple_person.index =
      Option.value ~default:0l resp.index;
    sex = sex_to_v1 resp.sex;
    lastname = sn;
    firstname = fn;
    n;
    p;
    occ;
    birth_short_date;
    birth_date_raw;
    birth_place;
    death_short_date;
    death_date_raw;
    death_place;
    image = resp.image;
    sosa = sosa_of_response resp;
    sosa_nb;
    visible_for_visitors;
    baseprefix = conf.Geneweb.Config.command;
    has_parent;
    has_spouse;
    has_child;
    is_contemporary = Option.value ~default:false resp.is_contemporary;
    name_is_hidden = is_hidden;
    name_is_restricted = false;
  }

let person_of_simple (s : Api_saisie_read_piqi.Simple_person.t) :
    Api_saisie_read_piqi.Person.t =
  let p = Api_saisie_read_piqi.default_person () in
  {
    p with
    type_ = `simple;
    index = s.index;
    sex = s.sex;
    lastname = s.lastname;
    firstname = s.firstname;
    n = s.n;
    p = s.p;
    occ = s.occ;
    image = s.image;
    birth_date = s.birth_short_date;
    birth_place = s.birth_place;
    death_date = s.death_short_date;
    death_place = s.death_place;
    sosa = s.sosa;
    baseprefix = s.baseprefix;
    is_contemporary = s.is_contemporary;
    name_is_hidden = s.name_is_hidden;
    name_is_restricted = s.name_is_restricted;
  }

let marriage_type_to_v1 = function
  | Some `married -> `married
  | Some `not_married -> `not_married
  | Some `engaged -> `engaged
  | Some `no_sexes_check_not_married -> `no_sexes_check_not_married
  | Some `no_mention -> `no_mention
  | Some `no_sexes_check_married -> `no_sexes_check_married
  | Some `marriage_bann -> `marriage_bann
  | Some `marriage_contract -> `marriage_contract
  | Some `marriage_license -> `marriage_license
  | Some `pacs -> `pacs
  | Some `residence -> `residence
  | None -> `no_mention

let divorce_type_to_v1 = function
  | Some `not_divorced -> `not_divorced
  | Some `divorced -> `divorced
  | Some `separated -> `separated
  | None -> `not_divorced

let fiche_family_of_family conf (fam : Common.family) :
    Api_saisie_read_piqi.Fiche_family.t =
  let default_fam = Api_saisie_read_piqi.default_fiche_family () in
  let spouse =
    match fam.fam_spouse with
    | Some sp ->
        let simple_sp =
          simple_person_of_response conf (Response.response_of_person sp)
        in
        person_of_simple simple_sp
    | None -> default_fam.spouse
  in
  let children =
    match fam.children with
    | Some ch_list ->
        List.map
          (fun ch ->
            person_of_simple
              (simple_person_of_response conf (Response.response_of_person ch)))
          ch_list
    | None -> []
  in
  {
    default_fam with
    index = Int32.of_int fam.index;
    spouse;
    marriage_date = fam.marriage_date_str;
    marriage_date_long = fam.marriage_date_long;
    marriage_date_raw = fam.marriage_date_raw;
    marriage_date_conv = fam.marriage_date_conv;
    marriage_date_conv_long = fam.marriage_date_conv_long;
    marriage_place = fam.marriage_place;
    marriage_src = fam.marriage_source;
    marriage_type = marriage_type_to_v1 fam.marriage_type;
    divorce_type = divorce_type_to_v1 fam.divorce_type;
    divorce_date = fam.divorce_date_str;
    divorce_date_long = fam.divorce_date_long;
    divorce_date_raw = fam.divorce_date_raw;
    divorce_date_conv = fam.divorce_date_conv;
    divorce_date_conv_long = fam.divorce_date_conv_long;
    notes = fam.marriage_note;
    fsources = fam.marriage_source;
    children;
  }

let event_type_to_v1 (ev_name : Common.event_type) :
    Api_saisie_read_piqi.event_type =
  match ev_name with
  | Geneweb.Event.Pevent pe -> Api_piqi_util.piqi_pevent_name_of_pevent_name pe
  | Geneweb.Event.Fevent fe -> Api_piqi_util.piqi_fevent_name_of_fevent_name fe

let fiche_event_of_event conf (evt : Common.event) :
    Api_saisie_read_piqi.Fiche_event.t =
  let default_evt = Api_saisie_read_piqi.default_fiche_event () in
  let spouse =
    Option.map
      (fun sp ->
        person_of_simple
          (simple_person_of_response conf (Response.response_of_person sp)))
      evt.spouse
  in
  {
    default_evt with
    name = "";
    type_ = event_type_to_v1 evt.event_type;
    date = evt.date_str;
    date_long = evt.date_long;
    date_raw = evt.date_raw;
    date_conv = evt.date_conv;
    date_conv_long = evt.date_conv_long;
    place = evt.place;
    note = evt.notes;
    src = evt.sources;
    spouse;
  }

let fiche_person_of_response conf _base (resp : Response.t) :
    Api_saisie_read_piqi.Fiche_person.t =
  let burial_type =
    match resp.burial with
    | Some { burial_type = Some (Def.Buried _); _ } -> `buried
    | Some { burial_type = Some (Def.Cremated _); _ } -> `cremated
    | _ -> `dont_know
  in
  let father =
    Option.map
      (fun f -> person_of_simple (simple_person_of_response conf f))
      resp.father
  in
  let mother =
    Option.map
      (fun m -> person_of_simple (simple_person_of_response conf m))
      resp.mother
  in
  let families =
    match resp.families with
    | Some fams -> List.map (fiche_family_of_family conf) fams
    | None -> []
  in
  let events =
    match resp.events with
    | Some evts -> List.map (fiche_event_of_event conf) evts.elements
    | None -> []
  in
  let f = Api_saisie_read_piqi.default_fiche_person () in
  {
    f with
    birth_date_raw = Option.bind resp.birth (fun e -> e.date_raw);
    birth_text = Option.bind resp.birth (fun e -> e.notes);
    baptism_date_raw = Option.bind resp.baptism (fun e -> e.date_raw);
    baptism_text = Option.bind resp.baptism (fun e -> e.notes);
    death_date_raw = Option.bind resp.death (fun e -> e.date_raw);
    death_text = Option.bind resp.death (fun e -> e.notes);
    burial_date_raw = Option.bind resp.burial (fun e -> e.date_raw);
    burial_text = Option.bind resp.burial (fun e -> e.notes);
    burial_type;
    titles_links = Option.value ~default:[] resp.titles;
    sosa_nb = resp.sosa;
    visible_for_visitors = visibility_to_v1 resp.visibility;
    father;
    mother;
    families;
    events;
    is_contemporary = Option.value ~default:false resp.is_contemporary;
  }

let family_of_v2_family conf (fam : Common.family) :
    Api_saisie_read_piqi.Family.t =
  let default_fam = Api_saisie_read_piqi.default_family () in
  let spouse =
    match fam.fam_spouse with
    | Some sp -> simple_person_of_response conf (Response.response_of_person sp)
    | None -> Api_saisie_read_piqi.default_simple_person ()
  in
  let children =
    match fam.children with
    | Some ch_list ->
        List.map
          (fun ch ->
            simple_person_of_response conf (Response.response_of_person ch))
          ch_list
    | None -> []
  in
  {
    default_fam with
    index = Int32.of_int fam.index;
    spouse;
    marriage_date = fam.marriage_date_str;
    marriage_date_long = fam.marriage_date_long;
    marriage_date_raw = fam.marriage_date_raw;
    marriage_date_conv = fam.marriage_date_conv;
    marriage_date_conv_long = fam.marriage_date_conv_long;
    marriage_place = fam.marriage_place;
    marriage_src = fam.marriage_source;
    marriage_type = marriage_type_to_v1 fam.marriage_type;
    divorce_type = divorce_type_to_v1 fam.divorce_type;
    divorce_date = fam.divorce_date_str;
    divorce_date_long = fam.divorce_date_long;
    divorce_date_raw = fam.divorce_date_raw;
    divorce_date_conv = fam.divorce_date_conv;
    divorce_date_conv_long = fam.divorce_date_conv_long;
    notes = fam.marriage_note;
    fsources = fam.marriage_source;
    children;
  }

let person_of_response conf base (resp : Response.t) :
    Api_saisie_read_piqi.Person.t =
  let simple_p = simple_person_of_response conf resp in
  let fiche_p = fiche_person_of_response conf base resp in
  let p = person_of_simple simple_p in
  let simple_families =
    match resp.families with
    | Some fams -> List.map (family_of_v2_family conf) fams
    | None -> []
  in
  let father =
    Option.map (fun f -> simple_person_of_response conf f) resp.father
  in
  let mother =
    Option.map (fun m -> simple_person_of_response conf m) resp.mother
  in
  {
    p with
    type_ = `fiche;
    fiche_person_person = Some fiche_p;
    families = simple_families;
    father;
    mother;
  }

let print_fiche_person_emulated conf base =
  let params =
    Api_util.get_params conf Api_saisie_read_piqi_ext.parse_fiche_parameters
  in
  match request_of_fiche_parameters params with
  | None ->
      Geneweb.Output.status conf Def.Not_Found;
      Geneweb.Output.print_sstring conf ""
  | Some req -> (
      match Response.response conf base req with
      | None ->
          Geneweb.Output.status conf Def.Not_Found;
          Geneweb.Output.print_sstring conf ""
      | Some resp ->
          let person_piqi = person_of_response conf base resp in
          let data = Api_saisie_read_piqi_ext.gen_person person_piqi in
          Api_util.print_result conf data)
