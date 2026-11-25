(** Unit test suite for API v2 endpoint, field extraction, and privacy filtering. *)

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
      ~surname:"Dupont" ~sex:Def.Male ~access:Def.Public
      ~titles:[ (Def.Tname "Chevalier", "chevalier", "du Temple") ]
      ()
  in
  let p_private =
    Geneweb_test_utils.Mock_db.add_person db ~first_name:"Secret"
      ~surname:"Agent" ~sex:Def.Female ~access:Def.Private ()
  in
  let _fam =
    Geneweb_test_utils.Mock_db.add_family db ~father:p_father ~mother:p_mother
      ~children:[ p_child ] ()
  in
  let base = Geneweb_test_utils.Mock_db.build db in
  (base, p_father, p_mother, p_child, p_private)

let conf_visitor =
  {
    Geneweb.Config.empty with
    wizard = false;
    friend = false;
    hide_private_names = true;
  }

let conf_wizard =
  {
    Geneweb.Config.empty with
    wizard = true;
    friend = false;
    hide_private_names = true;
  }

let test_lookup_by_index () =
  let base, _p_father, _p_mother, p_child, _p_private = setup_db () in
  let req =
    Plugin_api_lib.Request.request_of_piqi_request
      {
        Plugin_api_lib.Api_v2_piqi.Request.index = Some (Int32.of_int p_child);
        npocc = None;
        person_request = None;
      }
  in
  Alcotest.(check bool) "request parsed" true (Option.is_some req);
  let req = Option.get req in
  let resp = Plugin_api_lib.Response.response conf_visitor base req in
  Alcotest.(check bool) "response found" true (Option.is_some resp);
  let piqi = Plugin_api_lib.Response.to_piqi (Option.get resp) in
  Alcotest.(check (option string))
    "first name matches" (Some "Pierre") piqi.firstname;
  Alcotest.(check (option string))
    "surname matches" (Some "Dupont") piqi.lastname;
  Alcotest.(check bool) "sex matches male" true (piqi.sex = Some `male)

let test_lookup_by_npocc () =
  let base, _p_father, _p_mother, _p_child, _p_private = setup_db () in
  let npocc =
    {
      Plugin_api_lib.Api_v2_piqi.Npocc.n = Some "jean";
      p = Some "dupont";
      occ = Some 0l;
    }
  in
  let req =
    Plugin_api_lib.Request.request_of_piqi_request
      {
        Plugin_api_lib.Api_v2_piqi.Request.index = None;
        npocc = Some npocc;
        person_request = None;
      }
  in
  Alcotest.(check bool) "request parsed" true (Option.is_some req);
  let resp =
    Plugin_api_lib.Response.response conf_visitor base (Option.get req)
  in
  Alcotest.(check bool) "response found" true (Option.is_some resp);
  let piqi = Plugin_api_lib.Response.to_piqi (Option.get resp) in
  Alcotest.(check (option string))
    "first name matches" (Some "Jean") piqi.firstname;
  Alcotest.(check (option string))
    "surname matches" (Some "Dupont") piqi.lastname

let test_lookup_not_found () =
  let base, _p_father, _p_mother, _p_child, _p_private = setup_db () in
  let req =
    Plugin_api_lib.Request.request_of_piqi_request
      {
        Plugin_api_lib.Api_v2_piqi.Request.index = Some 999999l;
        npocc = None;
        person_request = None;
      }
  in
  Alcotest.(check bool) "request parsed" true (Option.is_some req);
  let resp =
    Plugin_api_lib.Response.response conf_visitor base (Option.get req)
  in
  Alcotest.(check bool) "response not found" true (Option.is_none resp)

let test_parent_hierarchy () =
  let base, _p_father, _p_mother, p_child, _p_private = setup_db () in
  let empty_person_req =
    {
      Plugin_api_lib.Api_v2_piqi.Person_request.index = Some true;
      npocc = None;
      lastname = Some true;
      firstname = Some true;
      sex = None;
      image = None;
      public_name = None;
      name_aliases = None;
      father = None;
      mother = None;
      sosa = None;
      occupation = None;
      notes = None;
      sources = None;
      titles = None;
      events = None;
      birth = None;
      baptism = None;
      death = None;
      burial = None;
      related = None;
      rparents = None;
      families = None;
    }
  in
  let person_req =
    {
      empty_person_req with
      father = Some empty_person_req;
      mother = Some empty_person_req;
    }
  in
  let req =
    Plugin_api_lib.Request.request_of_piqi_request
      {
        Plugin_api_lib.Api_v2_piqi.Request.index = Some (Int32.of_int p_child);
        npocc = None;
        person_request = Some person_req;
      }
  in
  let resp =
    Plugin_api_lib.Response.response conf_visitor base (Option.get req)
  in
  let piqi = Plugin_api_lib.Response.to_piqi (Option.get resp) in
  Alcotest.(check bool) "father populated" true (Option.is_some piqi.father);
  Alcotest.(check bool) "mother populated" true (Option.is_some piqi.mother);
  let father = Option.get piqi.father in
  let mother = Option.get piqi.mother in
  Alcotest.(check (option string))
    "father firstname" (Some "Jean") father.firstname;
  Alcotest.(check (option string))
    "father lastname" (Some "Dupont") father.lastname;
  Alcotest.(check (option string))
    "mother firstname" (Some "Marie") mother.firstname;
  Alcotest.(check (option string))
    "mother lastname" (Some "Martin") mother.lastname

let test_privacy_filtering () =
  let base, _p_father, _p_mother, _p_child, p_private = setup_db () in
  let req =
    Plugin_api_lib.Request.request_of_piqi_request
      {
        Plugin_api_lib.Api_v2_piqi.Request.index = Some (Int32.of_int p_private);
        npocc = None;
        person_request = None;
      }
  in
  let resp_visitor =
    Plugin_api_lib.Response.response conf_visitor base (Option.get req)
  in
  let piqi_visitor =
    Plugin_api_lib.Response.to_piqi (Option.get resp_visitor)
  in
  Alcotest.(check (option string))
    "hidden firstname for visitor" None piqi_visitor.firstname;
  Alcotest.(check (option string))
    "hidden lastname for visitor" None piqi_visitor.lastname;
  let resp_wizard =
    Plugin_api_lib.Response.response conf_wizard base (Option.get req)
  in
  let piqi_wizard = Plugin_api_lib.Response.to_piqi (Option.get resp_wizard) in
  Alcotest.(check (option string))
    "revealed firstname for wizard" (Some "Secret") piqi_wizard.firstname;
  Alcotest.(check (option string))
    "revealed lastname for wizard" (Some "Agent") piqi_wizard.lastname

let test_titles_extraction () =
  let base, _p_father, _p_mother, p_child, _p_private = setup_db () in
  let person_req =
    {
      Plugin_api_lib.Api_v2_piqi.Person_request.index = Some true;
      npocc = None;
      lastname = Some true;
      firstname = Some true;
      sex = None;
      image = None;
      public_name = None;
      name_aliases = None;
      father = None;
      mother = None;
      sosa = None;
      occupation = None;
      notes = None;
      sources = None;
      titles = Some true;
      events = None;
      birth = None;
      baptism = None;
      death = None;
      burial = None;
      related = None;
      rparents = None;
      families = None;
    }
  in
  let req =
    Plugin_api_lib.Request.request_of_piqi_request
      {
        Plugin_api_lib.Api_v2_piqi.Request.index = Some (Int32.of_int p_child);
        npocc = None;
        person_request = Some person_req;
      }
  in
  let resp =
    Plugin_api_lib.Response.response conf_visitor base (Option.get req)
  in
  let piqi = Plugin_api_lib.Response.to_piqi (Option.get resp) in
  Alcotest.(check int) "titles count" 1 (List.length piqi.titles)

let test_default_request_is_full_fields () =
  let base, _p_father, _p_mother, p_child, _p_private = setup_db () in
  let req =
    Plugin_api_lib.Request.request_of_piqi_request
      {
        Plugin_api_lib.Api_v2_piqi.Request.index = Some (Int32.of_int p_child);
        npocc = None;
        person_request = None;
      }
  in
  let resp =
    Plugin_api_lib.Response.response conf_visitor base (Option.get req)
  in
  Alcotest.(check bool) "response found" true (Option.is_some resp);
  let piqi = Plugin_api_lib.Response.to_piqi (Option.get resp) in
  Alcotest.(check (option string)) "firstname" (Some "Pierre") piqi.firstname;
  Alcotest.(check (option string)) "lastname" (Some "Dupont") piqi.lastname;
  Alcotest.(check bool)
    "father is present with default full fields" true
    (Option.is_some piqi.father);
  Alcotest.(check bool)
    "mother is present with default full fields" true
    (Option.is_some piqi.mother)

let tests =
  [
    ("Api_v2.lookup_by_index", `Quick, test_lookup_by_index);
    ("Api_v2.lookup_by_npocc", `Quick, test_lookup_by_npocc);
    ("Api_v2.lookup_not_found", `Quick, test_lookup_not_found);
    ("Api_v2.parent_hierarchy", `Quick, test_parent_hierarchy);
    ("Api_v2.privacy_filtering", `Quick, test_privacy_filtering);
    ("Api_v2.titles_extraction", `Quick, test_titles_extraction);
    ( "Api_v2.default_request_is_full_fields",
      `Quick,
      test_default_request_is_full_fields );
  ]
