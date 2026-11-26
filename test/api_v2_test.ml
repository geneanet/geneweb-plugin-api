(** Unit test suite for API v2 endpoint, field extraction, and privacy filtering. *)

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
  let bname = Filename.temp_file "gw_api_v2_" "" in
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
          ~surname:"Dupont" ~sex:Def.Male ~access:Def.Public
          ~titles:[ (Def.Tname "Chevalier", "chevalier", "du Temple") ]
          ()
      in
      let p_private =
        Geneweb_test_utils.Mock_db.add_person db ~first_name:"Secret"
          ~surname:"Agent" ~sex:Def.Female ~access:Def.Private ()
      in
      let _fam =
        Geneweb_test_utils.Mock_db.add_family db ~father:p_father
          ~mother:p_mother ~children:[ p_child ] ()
      in
      let base = Geneweb_test_utils.Mock_db.build ~bname db in
      f base p_father p_mother p_child p_private)

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
  with_base (fun base _p_father _p_mother p_child _p_private ->
      let req =
        Plugin_api_lib.Request.request_of_piqi_request
          {
            Plugin_api_lib.Api_v2_piqi.Request.index =
              Some (Int32.of_int p_child);
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
      Alcotest.(check bool) "sex matches male" true (piqi.sex = Some `male))

let test_lookup_by_npocc () =
  with_base (fun base _p_father _p_mother _p_child _p_private ->
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
        "surname matches" (Some "Dupont") piqi.lastname)

let test_lookup_not_found () =
  with_base (fun base _p_father _p_mother _p_child _p_private ->
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
      Alcotest.(check bool) "response not found" true (Option.is_none resp))

let test_parent_hierarchy () =
  with_base (fun base _p_father _p_mother p_child _p_private ->
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
          notes_raw = None;
          visibility = None;
          is_contemporary = None;
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
            Plugin_api_lib.Api_v2_piqi.Request.index =
              Some (Int32.of_int p_child);
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
        "mother lastname" (Some "Martin") mother.lastname)

let test_privacy_filtering () =
  with_base (fun base _p_father _p_mother _p_child p_private ->
      let req =
        Plugin_api_lib.Request.request_of_piqi_request
          {
            Plugin_api_lib.Api_v2_piqi.Request.index =
              Some (Int32.of_int p_private);
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
      let piqi_wizard =
        Plugin_api_lib.Response.to_piqi (Option.get resp_wizard)
      in
      Alcotest.(check (option string))
        "revealed firstname for wizard" (Some "Secret") piqi_wizard.firstname;
      Alcotest.(check (option string))
        "revealed lastname for wizard" (Some "Agent") piqi_wizard.lastname)

let test_titles_extraction () =
  with_base (fun base _p_father _p_mother p_child _p_private ->
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
          notes_raw = None;
          visibility = None;
          is_contemporary = None;
        }
      in
      let req =
        Plugin_api_lib.Request.request_of_piqi_request
          {
            Plugin_api_lib.Api_v2_piqi.Request.index =
              Some (Int32.of_int p_child);
            npocc = None;
            person_request = Some person_req;
          }
      in
      let resp =
        Plugin_api_lib.Response.response conf_visitor base (Option.get req)
      in
      let piqi = Plugin_api_lib.Response.to_piqi (Option.get resp) in
      Alcotest.(check int) "titles count" 1 (List.length piqi.titles))

let test_default_request_is_full_fields () =
  with_base (fun base _p_father _p_mother p_child _p_private ->
      let req =
        Plugin_api_lib.Request.request_of_piqi_request
          {
            Plugin_api_lib.Api_v2_piqi.Request.index =
              Some (Int32.of_int p_child);
            npocc = None;
            person_request = None;
          }
      in
      let resp =
        Plugin_api_lib.Response.response conf_visitor base (Option.get req)
      in
      Alcotest.(check bool) "response found" true (Option.is_some resp);
      let piqi = Plugin_api_lib.Response.to_piqi (Option.get resp) in
      Alcotest.(check (option string))
        "firstname" (Some "Pierre") piqi.firstname;
      Alcotest.(check (option string)) "lastname" (Some "Dupont") piqi.lastname;
      Alcotest.(check bool)
        "father is present with default full fields" true
        (Option.is_some piqi.father);
      Alcotest.(check bool)
        "mother is present with default full fields" true
        (Option.is_some piqi.mother))

let test_family_extraction () =
  with_base (fun base p_father p_mother _p_child _p_private ->
      (* Test father-centric family extraction *)
      let req_father =
        Plugin_api_lib.Request.request_of_piqi_request
          {
            Plugin_api_lib.Api_v2_piqi.Request.index =
              Some (Int32.of_int p_father);
            npocc = None;
            person_request = None;
          }
      in
      let resp_father =
        Plugin_api_lib.Response.response conf_visitor base
          (Option.get req_father)
      in
      Alcotest.(check bool)
        "father response found" true
        (Option.is_some resp_father);
      let piqi_father =
        Plugin_api_lib.Response.to_piqi (Option.get resp_father)
      in
      Alcotest.(check int)
        "father families count" 1
        (List.length piqi_father.families);
      let fam_father = List.hd piqi_father.families in
      Alcotest.(check bool)
        "spouse present for father" true
        (Option.is_some fam_father.spouse);
      let spouse_father = Option.get fam_father.spouse in
      Alcotest.(check (option string))
        "spouse firstname from father query" (Some "Marie")
        spouse_father.firstname;
      Alcotest.(check (option string))
        "spouse lastname from father query" (Some "Martin")
        spouse_father.lastname;
      Alcotest.(check int)
        "father children count" 1
        (List.length fam_father.children);
      let child_father = List.hd fam_father.children in
      Alcotest.(check (option string))
        "child firstname from father query" (Some "Pierre")
        child_father.firstname;
      Alcotest.(check (option string))
        "child lastname from father query" (Some "Dupont") child_father.lastname;
      (* Symmetrical: Test mother-centric family extraction *)
      let req_mother =
        Plugin_api_lib.Request.request_of_piqi_request
          {
            Plugin_api_lib.Api_v2_piqi.Request.index =
              Some (Int32.of_int p_mother);
            npocc = None;
            person_request = None;
          }
      in
      let resp_mother =
        Plugin_api_lib.Response.response conf_visitor base
          (Option.get req_mother)
      in
      Alcotest.(check bool)
        "mother response found" true
        (Option.is_some resp_mother);
      let piqi_mother =
        Plugin_api_lib.Response.to_piqi (Option.get resp_mother)
      in
      Alcotest.(check int)
        "mother families count" 1
        (List.length piqi_mother.families);
      let fam_mother = List.hd piqi_mother.families in
      Alcotest.(check bool)
        "spouse present for mother" true
        (Option.is_some fam_mother.spouse);
      let spouse_mother = Option.get fam_mother.spouse in
      Alcotest.(check (option string))
        "spouse firstname from mother query" (Some "Jean")
        spouse_mother.firstname;
      Alcotest.(check (option string))
        "spouse lastname from mother query" (Some "Dupont")
        spouse_mother.lastname;
      Alcotest.(check int)
        "mother children count" 1
        (List.length fam_mother.children);
      let child_mother = List.hd fam_mother.children in
      Alcotest.(check (option string))
        "child firstname from mother query" (Some "Pierre")
        child_mother.firstname;
      Alcotest.(check (option string))
        "child lastname from mother query" (Some "Dupont") child_mother.lastname)

let test_multiple_unions () =
  let bname = Filename.temp_file "gw_api_v2_unions_" "" in
  Sys.remove bname;
  Fun.protect
    ~finally:(fun () -> clean_db bname)
    (fun () ->
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
      let base = Geneweb_test_utils.Mock_db.build ~bname db in
      let req =
        Plugin_api_lib.Request.request_of_piqi_request
          {
            Plugin_api_lib.Api_v2_piqi.Request.index =
              Some (Int32.of_int p_main);
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
      Alcotest.(check (option string))
        "second spouse" (Some "Marie") sp2.firstname;
      Alcotest.(check int) "fam1 has no children" 0 (List.length fam1.children);
      Alcotest.(check int) "fam2 has 1 child" 1 (List.length fam2.children))

let test_private_spouse_masking () =
  let bname = Filename.temp_file "gw_api_v2_priv_" "" in
  Sys.remove bname;
  Fun.protect
    ~finally:(fun () -> clean_db bname)
    (fun () ->
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
      let base = Geneweb_test_utils.Mock_db.build ~bname db in
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
      let piqi_wizard =
        Plugin_api_lib.Response.to_piqi (Option.get resp_wizard)
      in
      let fam_wizard = List.hd piqi_wizard.families in
      let sp_wizard = Option.get fam_wizard.spouse in
      Alcotest.(check (option string))
        "private spouse firstname revealed for wizard" (Some "Private")
        sp_wizard.firstname;
      Alcotest.(check (option string))
        "private spouse lastname revealed for wizard" (Some "Wife")
        sp_wizard.lastname)

let test_visibility_levels () =
  with_base (fun base _p_father _p_mother p_child p_private ->
      let req_child =
        Plugin_api_lib.Request.request_of_piqi_request
          {
            Plugin_api_lib.Api_v2_piqi.Request.index =
              Some (Int32.of_int p_child);
            npocc = None;
            person_request = None;
          }
      in
      let resp_child =
        Plugin_api_lib.Response.response conf_visitor base
          (Option.get req_child)
      in
      let piqi_child =
        Plugin_api_lib.Response.to_piqi (Option.get resp_child)
      in
      Alcotest.(check bool)
        "child has public visibility" true
        (piqi_child.visibility = Some `visibility_public);
      let req_priv =
        Plugin_api_lib.Request.request_of_piqi_request
          {
            Plugin_api_lib.Api_v2_piqi.Request.index =
              Some (Int32.of_int p_private);
            npocc = None;
            person_request = None;
          }
      in
      let resp_priv =
        Plugin_api_lib.Response.response conf_visitor base (Option.get req_priv)
      in
      let piqi_priv = Plugin_api_lib.Response.to_piqi (Option.get resp_priv) in
      Alcotest.(check bool)
        "private person has private visibility" true
        (piqi_priv.visibility = Some `visibility_private))

let test_family_sources () =
  let bname = Filename.temp_file "gw_api_v2_src_" "" in
  Sys.remove bname;
  Fun.protect
    ~finally:(fun () -> clean_db bname)
    (fun () ->
      let db = Geneweb_test_utils.Mock_db.create () in
      let p_h =
        Geneweb_test_utils.Mock_db.add_person db ~first_name:"Paul"
          ~surname:"Valery" ~sex:Def.Male ~access:Def.Public ()
      in
      let p_w =
        Geneweb_test_utils.Mock_db.add_person db ~first_name:"Jeanne"
          ~surname:"Gobillard" ~sex:Def.Female ~access:Def.Public ()
      in
      let _fam =
        Geneweb_test_utils.Mock_db.add_family db ~father:p_h ~mother:p_w
          ~fsources:"Archives de Paris" ()
      in
      let base = Geneweb_test_utils.Mock_db.build ~bname db in
      let req =
        Plugin_api_lib.Request.request_of_piqi_request
          {
            Plugin_api_lib.Api_v2_piqi.Request.index = Some (Int32.of_int p_h);
            npocc = None;
            person_request = None;
          }
      in
      let resp =
        Plugin_api_lib.Response.response conf_visitor base (Option.get req)
      in
      let piqi = Plugin_api_lib.Response.to_piqi (Option.get resp) in
      let fam = List.hd piqi.families in
      Alcotest.(check (option string))
        "marriage source matches" (Some "Archives de Paris") fam.marriage_source)

let test_notes_raw () =
  let bname = Filename.temp_file "gw_api_v2_notes_" "" in
  Sys.remove bname;
  Fun.protect
    ~finally:(fun () -> clean_db bname)
    (fun () ->
      let db = Geneweb_test_utils.Mock_db.create () in
      let s_idx =
        Geneweb_test_utils.Mock_db.add_string db "Raw Note Content\nLine 2"
      in
      let p =
        Geneweb_test_utils.Mock_db.add_person db ~first_name:"Charles"
          ~surname:"Baudelaire" ~sex:Def.Male ~access:Def.Public ()
      in
      let base = Geneweb_test_utils.Mock_db.build ~bname db in
      let ip = Gwdb.iper_of_int p in
      let p_data = Gwdb.gen_person_of_person (Gwdb.poi base ip) in
      Gwdb.patch_person base ip
        { p_data with notes = Geneweb_test_utils.Mock_db.istr s_idx };
      let req =
        Plugin_api_lib.Request.request_of_piqi_request
          {
            Plugin_api_lib.Api_v2_piqi.Request.index = Some (Int32.of_int p);
            npocc = None;
            person_request = None;
          }
      in
      let resp =
        Plugin_api_lib.Response.response conf_visitor base (Option.get req)
      in
      let piqi = Plugin_api_lib.Response.to_piqi (Option.get resp) in
      Alcotest.(check (option string))
        "notes_raw extracted" (Some "Raw Note Content\nLine 2") piqi.notes_raw)

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
    ("Api_v2.visibility_levels", `Quick, test_visibility_levels);
    ("Api_v2.family_sources", `Quick, test_family_sources);
    ("Api_v2.notes_raw", `Quick, test_notes_raw);
  ]
