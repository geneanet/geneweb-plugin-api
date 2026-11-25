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

let test_family_extraction () =
  let base, p_father, _p_mother, _p_child, _p_private = setup_db () in
  let req =
    Plugin_api_lib.Request.request_of_piqi_request
      {
        Plugin_api_lib.Api_v2_piqi.Request.index = Some (Int32.of_int p_father);
        npocc = None;
        person_request = None;
      }
  in
  let resp =
    Plugin_api_lib.Response.response conf_visitor base (Option.get req)
  in
  Alcotest.(check bool) "response found" true (Option.is_some resp);
  let piqi = Plugin_api_lib.Response.to_piqi (Option.get resp) in
  Alcotest.(check int) "families count" 1 (List.length piqi.families);
  let fam = List.hd piqi.families in
  Alcotest.(check bool) "spouse present" true (Option.is_some fam.spouse);
  let spouse = Option.get fam.spouse in
  Alcotest.(check (option string))
    "spouse firstname" (Some "Marie") spouse.firstname;
  Alcotest.(check (option string))
    "spouse lastname" (Some "Martin") spouse.lastname;
  Alcotest.(check int) "children count" 1 (List.length fam.children);
  let child = List.hd fam.children in
  Alcotest.(check (option string))
    "child firstname" (Some "Pierre") child.firstname;
  Alcotest.(check (option string))
    "child lastname" (Some "Dupont") child.lastname

let test_multiple_unions () =
  let db = Geneweb_test_utils.Mock_db.create () in
  let p_main =
    Geneweb_test_utils.Mock_db.add_person db ~first_name:"Henri"
      ~surname:"Quatre" ~sex:Def.Male ~access:Def.Public ()
  in
  let p_sp1 =
    Geneweb_test_utils.Mock_db.add_person db ~first_name:"Marguerite"
      ~surname:"Valois" ~sex:Def.Female ~access:Def.Public ()
  in
  let p_sp2 =
    Geneweb_test_utils.Mock_db.add_person db ~first_name:"Marie"
      ~surname:"Medicis" ~sex:Def.Female ~access:Def.Public ()
  in
  let p_c2 =
    Geneweb_test_utils.Mock_db.add_person db ~first_name:"Louis"
      ~surname:"Treize" ~sex:Def.Male ~access:Def.Public ()
  in
  let _fam1 =
    Geneweb_test_utils.Mock_db.add_family db ~father:p_main ~mother:p_sp1 ()
  in
  let _fam2 =
    Geneweb_test_utils.Mock_db.add_family db ~father:p_main ~mother:p_sp2
      ~children:[ p_c2 ] ()
  in
  let base = Geneweb_test_utils.Mock_db.build db in
  let req =
    Plugin_api_lib.Request.request_of_piqi_request
      {
        Plugin_api_lib.Api_v2_piqi.Request.index = Some (Int32.of_int p_main);
        npocc = None;
        person_request = None;
      }
  in
  let resp =
    Plugin_api_lib.Response.response conf_visitor base (Option.get req)
  in
  let piqi = Plugin_api_lib.Response.to_piqi (Option.get resp) in
  Alcotest.(check int) "two families" 2 (List.length piqi.families);
  let fam1 = List.nth piqi.families 0 in
  let fam2 = List.nth piqi.families 1 in
  let sp1 = Option.get fam1.spouse in
  let sp2 = Option.get fam2.spouse in
  Alcotest.(check (option string))
    "first spouse" (Some "Marguerite") sp1.firstname;
  Alcotest.(check (option string)) "second spouse" (Some "Marie") sp2.firstname;
  Alcotest.(check int) "fam1 has no children" 0 (List.length fam1.children);
  Alcotest.(check int) "fam2 has 1 child" 1 (List.length fam2.children)

let test_private_spouse_masking () =
  let db = Geneweb_test_utils.Mock_db.create () in
  let p_pub =
    Geneweb_test_utils.Mock_db.add_person db ~first_name:"Public"
      ~surname:"Husband" ~sex:Def.Male ~access:Def.Public ()
  in
  let p_priv =
    Geneweb_test_utils.Mock_db.add_person db ~first_name:"Private"
      ~surname:"Wife" ~sex:Def.Female ~access:Def.Private ()
  in
  let _fam =
    Geneweb_test_utils.Mock_db.add_family db ~father:p_pub ~mother:p_priv ()
  in
  let base = Geneweb_test_utils.Mock_db.build db in
  let req =
    Plugin_api_lib.Request.request_of_piqi_request
      {
        Plugin_api_lib.Api_v2_piqi.Request.index = Some (Int32.of_int p_pub);
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
  let fam_visitor = List.hd piqi_visitor.families in
  let sp_visitor = Option.get fam_visitor.spouse in
  Alcotest.(check (option string))
    "private spouse firstname masked for visitor" None sp_visitor.firstname;
  Alcotest.(check (option string))
    "private spouse lastname masked for visitor" None sp_visitor.lastname;
  let resp_wizard =
    Plugin_api_lib.Response.response conf_wizard base (Option.get req)
  in
  let piqi_wizard = Plugin_api_lib.Response.to_piqi (Option.get resp_wizard) in
  let fam_wizard = List.hd piqi_wizard.families in
  let sp_wizard = Option.get fam_wizard.spouse in
  Alcotest.(check (option string))
    "private spouse firstname revealed for wizard" (Some "Private")
    sp_wizard.firstname;
  Alcotest.(check (option string))
    "private spouse lastname revealed for wizard" (Some "Wife")
    sp_wizard.lastname

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
    ("Api_v2.family_extraction", `Quick, test_family_extraction);
    ("Api_v2.multiple_unions", `Quick, test_multiple_unions);
    ("Api_v2.private_spouse_masking", `Quick, test_private_spouse_masking);
  ]
