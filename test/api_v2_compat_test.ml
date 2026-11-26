(** Unit test suite for Api_v2_compat response emulation adapter. *)

let clean_db bname =
  let remove_path p =
    if Sys.file_exists p then
      if Sys.is_directory p then (
        Array.iter
          (fun f ->
            let sub = Filename.concat p f in
            if Sys.file_exists sub && not (Sys.is_directory sub) then
              Sys.remove sub)
          (Sys.readdir p);
        Unix.rmdir p)
      else Sys.remove p
  in
  remove_path bname;
  remove_path (bname ^ ".gwb")

let with_base f =
  let bname = Filename.temp_file "gw_api_v2_compat_" "" in
  Sys.remove bname;
  Fun.protect
    ~finally:(fun () -> clean_db bname)
    (fun () ->
      let db = Geneweb_test_utils.Mock_db.create () in
      let p_father =
        Geneweb_test_utils.Mock_db.add_person db ~first_name:"Jean"
          ~surname:"Dupont" ~sex:Def.Male ~access:Def.Public ()
      in
      let p_mother =
        Geneweb_test_utils.Mock_db.add_person db ~first_name:"Marie"
          ~surname:"Martin" ~sex:Def.Female ~access:Def.Public ()
      in
      let p_child =
        Geneweb_test_utils.Mock_db.add_person db ~first_name:"Pierre"
          ~surname:"Dupont" ~sex:Def.Male ~access:Def.Public ()
      in
      let _fam =
        Geneweb_test_utils.Mock_db.add_family db ~father:p_father
          ~mother:p_mother ~children:[ p_child ]
          ~fsources:"Archives municipales" ()
      in
      let base = Geneweb_test_utils.Mock_db.build ~bname db in
      f base p_father p_mother p_child)

let conf_visitor =
  {
    Geneweb.Config.empty with
    wizard = false;
    friend = false;
    hide_private_names = true;
  }

let test_request_translation () =
  let ident =
    {
      Plugin_api_lib.Api_saisie_read_piqi.Identifier_person.index = Some 42l;
      oc = None;
      n = None;
      p = None;
      track_visit = None;
    }
  in
  let params =
    {
      Plugin_api_lib.Api_saisie_read_piqi.Fiche_parameters.identifier_person =
        ident;
      nb_asc_max = Some 2l;
      nb_desc_max = Some 1l;
      simple_graph_info = Some false;
      no_event = Some false;
    }
  in
  let req = Plugin_api_lib.Api_v2_compat.request_of_fiche_parameters params in
  Geneweb_test_utils.Check.bool ~pos:__POS__
    ~msg:"Api_v2_compat.request_of_fiche_parameters translates valid parameters"
    true (Option.is_some req);
  let req = Option.get req in
  let fields = Plugin_api_lib.Request.get_fields req in
  Geneweb_test_utils.Check.bool ~pos:__POS__
    ~msg:"Api_v2_compat.request_of_fiche_parameters requests father field" true
    (Option.is_some fields.father);
  Geneweb_test_utils.Check.bool ~pos:__POS__
    ~msg:"Api_v2_compat.request_of_fiche_parameters requests mother field" true
    (Option.is_some fields.mother);
  Geneweb_test_utils.Check.bool ~pos:__POS__
    ~msg:"Api_v2_compat.request_of_fiche_parameters requests families field"
    true
    (Option.is_some fields.families);
  Geneweb_test_utils.Check.bool ~pos:__POS__
    ~msg:"Api_v2_compat.request_of_fiche_parameters requests events field" true
    (Option.is_some fields.events)

let test_emulate_fiche_person () =
  with_base (fun base _p_father _p_mother p_child ->
      let ident =
        {
          Plugin_api_lib.Api_saisie_read_piqi.Identifier_person.index =
            Some (Int32.of_int p_child);
          oc = None;
          n = None;
          p = None;
          track_visit = None;
        }
      in
      let params =
        {
          Plugin_api_lib.Api_saisie_read_piqi.Fiche_parameters.identifier_person =
            ident;
          nb_asc_max = None;
          nb_desc_max = None;
          simple_graph_info = None;
          no_event = None;
        }
      in
      let req =
        Option.get
          (Plugin_api_lib.Api_v2_compat.request_of_fiche_parameters params)
      in
      let resp =
        Option.get (Plugin_api_lib.Response.response conf_visitor base req)
      in
      let fiche =
        Plugin_api_lib.Api_v2_compat.fiche_person_of_response conf_visitor base
          resp
      in
      Geneweb_test_utils.Check.bool ~pos:__POS__
        ~msg:"Api_v2_compat.fiche_person_of_response includes father in fiche"
        true
        (Option.is_some fiche.father);
      Geneweb_test_utils.Check.bool ~pos:__POS__
        ~msg:"Api_v2_compat.fiche_person_of_response includes mother in fiche"
        true
        (Option.is_some fiche.mother);
      let father = Option.get fiche.father in
      let mother = Option.get fiche.mother in
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Api_v2_compat.fiche_person_of_response preserves father firstname"
        "Jean" father.firstname;
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Api_v2_compat.fiche_person_of_response preserves father lastname"
        "Dupont" father.lastname;
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Api_v2_compat.fiche_person_of_response preserves mother firstname"
        "Marie" mother.firstname;
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Api_v2_compat.fiche_person_of_response preserves mother lastname"
        "Martin" mother.lastname)

let test_emulate_person_wrapper () =
  with_base (fun base p_father _p_mother _p_child ->
      let ident =
        {
          Plugin_api_lib.Api_saisie_read_piqi.Identifier_person.index =
            Some (Int32.of_int p_father);
          oc = None;
          n = None;
          p = None;
          track_visit = None;
        }
      in
      let params =
        {
          Plugin_api_lib.Api_saisie_read_piqi.Fiche_parameters.identifier_person =
            ident;
          nb_asc_max = None;
          nb_desc_max = None;
          simple_graph_info = None;
          no_event = None;
        }
      in
      let req =
        Option.get
          (Plugin_api_lib.Api_v2_compat.request_of_fiche_parameters params)
      in
      let resp =
        Option.get (Plugin_api_lib.Response.response conf_visitor base req)
      in
      let person_piqi =
        Plugin_api_lib.Api_v2_compat.person_of_response conf_visitor base resp
      in
      Geneweb_test_utils.Check.bool ~pos:__POS__
        ~msg:"Api_v2_compat.person_of_response sets person type to fiche" true
        (person_piqi.type_ = `fiche);
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Api_v2_compat.person_of_response sets firstname" "Jean"
        person_piqi.firstname;
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Api_v2_compat.person_of_response sets lastname" "Dupont"
        person_piqi.lastname;
      Geneweb_test_utils.Check.bool ~pos:__POS__
        ~msg:"Api_v2_compat.person_of_response populates fiche extension" true
        (Option.is_some person_piqi.fiche_person_person);
      Geneweb_test_utils.Check.int ~pos:__POS__
        ~msg:"Api_v2_compat.person_of_response families count" 1
        (List.length person_piqi.families);
      let fam = List.hd person_piqi.families in
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Api_v2_compat.person_of_response spouse firstname" "Marie"
        fam.spouse.firstname;
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Api_v2_compat.person_of_response spouse lastname" "Martin"
        fam.spouse.lastname;
      Geneweb_test_utils.Check.int ~pos:__POS__
        ~msg:"Api_v2_compat.person_of_response children count" 1
        (List.length fam.children);
      let child = List.hd fam.children in
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Api_v2_compat.person_of_response child firstname" "Pierre"
        child.firstname;
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Api_v2_compat.person_of_response child lastname" "Dupont"
        child.lastname)

let test_request_translation_missing_identifier () =
  let ident =
    {
      Plugin_api_lib.Api_saisie_read_piqi.Identifier_person.index = None;
      oc = None;
      n = None;
      p = None;
      track_visit = None;
    }
  in
  let params =
    {
      Plugin_api_lib.Api_saisie_read_piqi.Fiche_parameters.identifier_person =
        ident;
      nb_asc_max = None;
      nb_desc_max = None;
      simple_graph_info = None;
      no_event = None;
    }
  in
  let req = Plugin_api_lib.Api_v2_compat.request_of_fiche_parameters params in
  Geneweb_test_utils.Check.bool ~pos:__POS__
    ~msg:
      "Api_v2_compat.request_of_fiche_parameters returns None for missing \
       identifier"
    true (Option.is_none req)

let test_emulate_person_not_found () =
  with_base (fun base _p_father _p_mother _p_child ->
      let ident =
        {
          Plugin_api_lib.Api_saisie_read_piqi.Identifier_person.index =
            Some 999999l;
          oc = None;
          n = None;
          p = None;
          track_visit = None;
        }
      in
      let params =
        {
          Plugin_api_lib.Api_saisie_read_piqi.Fiche_parameters.identifier_person =
            ident;
          nb_asc_max = None;
          nb_desc_max = None;
          simple_graph_info = None;
          no_event = None;
        }
      in
      let req =
        Plugin_api_lib.Api_v2_compat.request_of_fiche_parameters params
      in
      Geneweb_test_utils.Check.bool ~pos:__POS__
        ~msg:
          "Api_v2_compat.request_of_fiche_parameters returns Some for indexed \
           query"
        true (Option.is_some req);
      let resp =
        Option.bind req (Plugin_api_lib.Response.response conf_visitor base)
      in
      Geneweb_test_utils.Check.bool ~pos:__POS__
        ~msg:"Response.response returns None for nonexistent person" true
        (Option.is_none resp))

let test_emulate_person_by_npoc () =
  with_base (fun base _p_father _p_mother _p_child ->
      let ident =
        {
          Plugin_api_lib.Api_saisie_read_piqi.Identifier_person.index = None;
          oc = Some 0l;
          n = Some "dupont";
          p = Some "jean";
          track_visit = None;
        }
      in
      let params =
        {
          Plugin_api_lib.Api_saisie_read_piqi.Fiche_parameters.identifier_person =
            ident;
          nb_asc_max = None;
          nb_desc_max = None;
          simple_graph_info = None;
          no_event = None;
        }
      in
      let req =
        Plugin_api_lib.Api_v2_compat.request_of_fiche_parameters params
      in
      Geneweb_test_utils.Check.bool ~pos:__POS__
        ~msg:
          "Api_v2_compat.request_of_fiche_parameters returns Some for npoc \
           query"
        true (Option.is_some req);
      let resp =
        Option.bind req (Plugin_api_lib.Response.response conf_visitor base)
      in
      Geneweb_test_utils.Check.bool ~pos:__POS__
        ~msg:"Response.response finds person by npoc query" true
        (Option.is_some resp);
      let person_piqi =
        Plugin_api_lib.Api_v2_compat.person_of_response conf_visitor base
          (Option.get resp)
      in
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Api_v2_compat.person_of_response preserves firstname" "Jean"
        person_piqi.firstname;
      Geneweb_test_utils.Check.string ~pos:__POS__
        ~msg:"Api_v2_compat.person_of_response preserves lastname" "Dupont"
        person_piqi.lastname)

let tests =
  [
    Geneweb_test_utils.Check.case "Api_v2_compat.request_translation"
      test_request_translation;
    Geneweb_test_utils.Check.case
      "Api_v2_compat.request_translation_missing_identifier"
      test_request_translation_missing_identifier;
    Geneweb_test_utils.Check.case "Api_v2_compat.emulate_fiche_person"
      test_emulate_fiche_person;
    Geneweb_test_utils.Check.case "Api_v2_compat.emulate_person_wrapper"
      test_emulate_person_wrapper;
    Geneweb_test_utils.Check.case "Api_v2_compat.emulate_person_not_found"
      test_emulate_person_not_found;
    Geneweb_test_utils.Check.case "Api_v2_compat.emulate_person_by_npoc"
      test_emulate_person_by_npoc;
  ]
