(** Unit test suite for Api_v2_compat response emulation adapter. *)

let setup_db () =
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
    Geneweb_test_utils.Mock_db.add_family db ~father:p_father ~mother:p_mother
      ~children:[ p_child ] ~fsources:"Archives municipales" ()
  in
  let base = Geneweb_test_utils.Mock_db.build db in
  (base, p_father, p_mother, p_child)

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
  Alcotest.(check bool) "request translated" true (Option.is_some req);
  let req = Option.get req in
  let fields = Plugin_api_lib.Request.get_fields req in
  Alcotest.(check bool) "father requested" true (Option.is_some fields.father);
  Alcotest.(check bool) "mother requested" true (Option.is_some fields.mother);
  Alcotest.(check bool)
    "families requested" true
    (Option.is_some fields.families);
  Alcotest.(check bool) "events requested" true (Option.is_some fields.events)

let test_emulate_fiche_person () =
  let base, _p_father, _p_mother, p_child = setup_db () in
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
    Option.get (Plugin_api_lib.Api_v2_compat.request_of_fiche_parameters params)
  in
  let resp =
    Option.get (Plugin_api_lib.Response.response conf_visitor base req)
  in
  let fiche =
    Plugin_api_lib.Api_v2_compat.fiche_person_of_response conf_visitor base resp
  in
  Alcotest.(check bool)
    "father present in fiche" true
    (Option.is_some fiche.father);
  Alcotest.(check bool)
    "mother present in fiche" true
    (Option.is_some fiche.mother);
  let father = Option.get fiche.father in
  let mother = Option.get fiche.mother in
  Alcotest.(check string) "father firstname" "Jean" father.firstname;
  Alcotest.(check string) "father lastname" "Dupont" father.lastname;
  Alcotest.(check string) "mother firstname" "Marie" mother.firstname;
  Alcotest.(check string) "mother lastname" "Martin" mother.lastname

let test_emulate_person_wrapper () =
  let base, p_father, _p_mother, _p_child = setup_db () in
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
    Option.get (Plugin_api_lib.Api_v2_compat.request_of_fiche_parameters params)
  in
  let resp =
    Option.get (Plugin_api_lib.Response.response conf_visitor base req)
  in
  let person_piqi =
    Plugin_api_lib.Api_v2_compat.person_of_response conf_visitor base resp
  in
  Alcotest.(check bool) "person type is fiche" true (person_piqi.type_ = `fiche);
  Alcotest.(check string) "firstname" "Jean" person_piqi.firstname;
  Alcotest.(check string) "lastname" "Dupont" person_piqi.lastname;
  Alcotest.(check bool)
    "fiche extension populated" true
    (Option.is_some person_piqi.fiche_person_person);
  let fiche = Option.get person_piqi.fiche_person_person in
  Alcotest.(check int) "families count" 1 (List.length fiche.families);
  let fam = List.hd fiche.families in
  Alcotest.(check string) "spouse firstname" "Marie" fam.spouse.firstname;
  Alcotest.(check string) "spouse lastname" "Martin" fam.spouse.lastname;
  Alcotest.(check int) "children count" 1 (List.length fam.children);
  let child = List.hd fam.children in
  Alcotest.(check string) "child firstname" "Pierre" child.firstname;
  Alcotest.(check string) "child lastname" "Dupont" child.lastname

let tests =
  [
    ("Api_v2_compat.request_translation", `Quick, test_request_translation);
    ("Api_v2_compat.emulate_fiche_person", `Quick, test_emulate_fiche_person);
    ("Api_v2_compat.emulate_person_wrapper", `Quick, test_emulate_person_wrapper);
  ]
