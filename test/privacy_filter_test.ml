let setup_db () =
  let db = Geneweb_test_utils.Mock_db.create () in
  let priv_id =
    Geneweb_test_utils.Mock_db.add_person db ~first_name:"John" ~surname:"Doe"
      ~access:Def.Private
      ~titles:[ (Def.Tname "Duke", "duke", "of York") ]
      ()
  in
  let pub_id =
    Geneweb_test_utils.Mock_db.add_person db ~first_name:"Jane" ~surname:"Smith"
      ~access:Def.Public
      ~titles:[ (Def.Tname "Countess", "countess", "of Chester") ]
      ()
  in
  let semi_id =
    Geneweb_test_utils.Mock_db.add_person db ~first_name:"Sam" ~surname:"Semi"
      ~access:Def.IfTitles
      ~titles:[ (Def.Tname "Baron", "baron", "of Halifax") ]
      ()
  in
  let base = Geneweb_test_utils.Mock_db.build db in
  ( base,
    Geneweb_test_utils.Mock_db.poi base priv_id,
    Geneweb_test_utils.Mock_db.poi base pub_id,
    Geneweb_test_utils.Mock_db.poi base semi_id )

let visitor_conf =
  { Geneweb.Config.empty with
    wizard = false;
    friend = false;
    hide_private_names = true;
  }

let visitor_show_names_conf =
  { Geneweb.Config.empty with
    wizard = false;
    friend = false;
    hide_private_names = false;
  }

let wizard_conf =
  { Geneweb.Config.empty with
    wizard = true;
    friend = false;
    hide_private_names = true;
  }

let make_gen_p ?(first_name = "First") ?(surname = "Last") ?(public_name = "")
    ?(aliases = []) ?(qualifiers = []) ?(first_names_aliases = [])
    ?(surnames_aliases = []) () =
  {
    (Mutil.empty_person "" "?") with
    Def.first_name;
    surname;
    public_name;
    aliases;
    qualifiers;
    first_names_aliases;
    surnames_aliases;
  }

let test_aliases () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Plugin_api_lib.Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base pub_p in
  let gen_p = make_gen_p ~aliases:[ "Alias1"; "Alias2" ] () in
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.aliases hides aliases for private person from visitor"
    []
    (Plugin_api_lib.Api_privacy_filter.aliases priv_vis gen_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.aliases reveals aliases for private person to wizard"
    [ "Alias1"; "Alias2" ]
    (Plugin_api_lib.Api_privacy_filter.aliases wiz_vis gen_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.aliases reveals aliases for public person to visitor"
    [ "Alias1"; "Alias2" ]
    (Plugin_api_lib.Api_privacy_filter.aliases pub_vis gen_p)

let test_first_names_aliases () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Plugin_api_lib.Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base pub_p in
  let gen_p = make_gen_p ~first_names_aliases:[ "Johnny"; "Jack" ] () in
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_names_aliases hides aliases for private person from visitor"
    []
    (Plugin_api_lib.Api_privacy_filter.first_names_aliases priv_vis gen_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_names_aliases reveals aliases for private person to wizard"
    [ "Johnny"; "Jack" ]
    (Plugin_api_lib.Api_privacy_filter.first_names_aliases wiz_vis gen_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_names_aliases reveals aliases for public person to visitor"
    [ "Johnny"; "Jack" ]
    (Plugin_api_lib.Api_privacy_filter.first_names_aliases pub_vis gen_p)

let test_surnames_aliases () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Plugin_api_lib.Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base pub_p in
  let gen_p = make_gen_p ~surnames_aliases:[ "Doeman" ] () in
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surnames_aliases hides aliases for private person from visitor"
    []
    (Plugin_api_lib.Api_privacy_filter.surnames_aliases priv_vis gen_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surnames_aliases reveals aliases for private person to wizard"
    [ "Doeman" ]
    (Plugin_api_lib.Api_privacy_filter.surnames_aliases wiz_vis gen_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surnames_aliases reveals aliases for public person to visitor"
    [ "Doeman" ]
    (Plugin_api_lib.Api_privacy_filter.surnames_aliases pub_vis gen_p)

let test_qualifiers () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Plugin_api_lib.Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base pub_p in
  let gen_p = make_gen_p ~qualifiers:[ "the Great" ] () in
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.qualifiers hides qualifiers for private person from visitor"
    []
    (Plugin_api_lib.Api_privacy_filter.qualifiers priv_vis gen_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.qualifiers reveals qualifiers for private person to wizard"
    [ "the Great" ]
    (Plugin_api_lib.Api_privacy_filter.qualifiers wiz_vis gen_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.qualifiers reveals qualifiers for public person to visitor"
    [ "the Great" ]
    (Plugin_api_lib.Api_privacy_filter.qualifiers pub_vis gen_p)

let test_public_name () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Plugin_api_lib.Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base pub_p in
  let gen_p = make_gen_p ~public_name:"Known Name" () in
  Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.public_name hides public name for private person from visitor"
    None
    (Plugin_api_lib.Api_privacy_filter.public_name priv_vis gen_p);
  Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.public_name reveals public name for private person to wizard"
    (Some "Known Name")
    (Plugin_api_lib.Api_privacy_filter.public_name wiz_vis gen_p);
  Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.public_name reveals public name for public person to visitor"
    (Some "Known Name")
    (Plugin_api_lib.Api_privacy_filter.public_name pub_vis gen_p)

let test_names () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Plugin_api_lib.Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base pub_p in
  let gen_p = make_gen_p ~first_name:"John" ~surname:"Doe" () in
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname masks private surname to x for visitor"
    "x"
    (Plugin_api_lib.Api_privacy_filter.surname priv_vis gen_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name masks private first name to x for visitor"
    "x"
    (Plugin_api_lib.Api_privacy_filter.first_name priv_vis gen_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname masks private surname with custom empty mask"
    ""
    (Plugin_api_lib.Api_privacy_filter.surname ~mask:"" priv_vis gen_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name masks private first name with custom empty mask"
    ""
    (Plugin_api_lib.Api_privacy_filter.first_name ~mask:"" priv_vis gen_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname reveals private surname to wizard"
    "Doe"
    (Plugin_api_lib.Api_privacy_filter.surname wiz_vis gen_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name reveals private first name to wizard"
    "John"
    (Plugin_api_lib.Api_privacy_filter.first_name wiz_vis gen_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname reveals public surname to visitor"
    "Doe"
    (Plugin_api_lib.Api_privacy_filter.surname pub_vis gen_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name reveals public first name to visitor"
    "John"
    (Plugin_api_lib.Api_privacy_filter.first_name pub_vis gen_p)

let test_names_lower () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Plugin_api_lib.Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base pub_p in
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname_lower returns empty string for private person to visitor"
    ""
    (Plugin_api_lib.Api_privacy_filter.surname_lower priv_vis base priv_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name_lower returns empty string for private person to visitor"
    ""
    (Plugin_api_lib.Api_privacy_filter.first_name_lower priv_vis base priv_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname_lower returns lowercased surname for private person to wizard"
    "doe"
    (Plugin_api_lib.Api_privacy_filter.surname_lower wiz_vis base priv_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name_lower returns lowercased first name for private person to wizard"
    "john"
    (Plugin_api_lib.Api_privacy_filter.first_name_lower wiz_vis base priv_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname_lower returns lowercased surname for public person to visitor"
    "smith"
    (Plugin_api_lib.Api_privacy_filter.surname_lower pub_vis base pub_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name_lower returns lowercased first name for public person to visitor"
    "jane"
    (Plugin_api_lib.Api_privacy_filter.first_name_lower pub_vis base pub_p)

let test_is_hidden () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Plugin_api_lib.Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base pub_p in
  Geneweb_test_utils.Check.bool ~pos:__POS__
    ~msg:"Api_privacy_filter.is_hidden returns true for private person to visitor"
    true
    (Plugin_api_lib.Api_privacy_filter.is_hidden priv_vis);
  Geneweb_test_utils.Check.bool ~pos:__POS__
    ~msg:"Api_privacy_filter.is_hidden returns false for private person to wizard"
    false
    (Plugin_api_lib.Api_privacy_filter.is_hidden wiz_vis);
  Geneweb_test_utils.Check.bool ~pos:__POS__
    ~msg:"Api_privacy_filter.is_hidden returns false for public person to visitor"
    false
    (Plugin_api_lib.Api_privacy_filter.is_hidden pub_vis);
  Geneweb_test_utils.Check.bool ~pos:__POS__
    ~msg:"Api_privacy_filter.is_visible returns false for private person to visitor"
    false
    (Plugin_api_lib.Api_privacy_filter.is_visible priv_vis);
  Geneweb_test_utils.Check.bool ~pos:__POS__
    ~msg:"Api_privacy_filter.is_visible returns true for private person to wizard"
    true
    (Plugin_api_lib.Api_privacy_filter.is_visible wiz_vis);
  Geneweb_test_utils.Check.bool ~pos:__POS__
    ~msg:"Api_privacy_filter.is_visible returns true for public person to visitor"
    true
    (Plugin_api_lib.Api_privacy_filter.is_visible pub_vis)

let test_titles () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Plugin_api_lib.Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Plugin_api_lib.Api_privacy_filter.evaluate visitor_conf base pub_p in
  (* When not visible (visitor on private person), always returns empty list *)
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.titles returns empty when not visible"
    []
    (Plugin_api_lib.Api_privacy_filter.titles priv_vis visitor_conf base priv_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.titles_with_links returns empty when not visible"
    []
    (Plugin_api_lib.Api_privacy_filter.titles_with_links priv_vis visitor_conf base priv_p);
  (* When visible to wizard on private person, reveals nobility titles *)
  let expected_priv = [ "duke of York (Duke)" ] in
  let expected_priv_links =
    [ "<a   href=\"?lang=en&m=TT&sm=S&t=duke&p=of+York\">duke of York</a> (Duke)" ]
  in
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.titles returns titles when visible to wizard"
    expected_priv
    (Plugin_api_lib.Api_privacy_filter.titles wiz_vis wizard_conf base priv_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.titles_with_links returns titles with links when visible to wizard"
    expected_priv_links
    (Plugin_api_lib.Api_privacy_filter.titles_with_links wiz_vis wizard_conf base priv_p);
  (* When visible to visitor on public person, reveals nobility titles *)
  let expected_pub = [ "countess of Chester (Countess)" ] in
  let expected_pub_links =
    [ "<a   href=\"?lang=en&m=TT&sm=S&t=countess&p=of+Chester\">countess of Chester</a> (Countess)" ]
  in
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.titles returns titles for public person when visible"
    expected_pub
    (Plugin_api_lib.Api_privacy_filter.titles pub_vis visitor_conf base pub_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.titles_with_links returns titles with links for public person when visible"
    expected_pub_links
    (Plugin_api_lib.Api_privacy_filter.titles_with_links pub_vis visitor_conf base pub_p)

let test_contemporary_semi_public () =
  let base, _, _, semi_p = setup_db () in
  let semi_vis =
    Plugin_api_lib.Api_privacy_filter.evaluate visitor_show_names_conf base semi_p
  in
  let gen_p =
    make_gen_p ~first_name:"Sam" ~surname:"Semi" ~public_name:"Lord Sam"
      ~aliases:[ "Sammy" ] ~qualifiers:[ "the Honest" ]
      ~first_names_aliases:[ "Samuel" ] ~surnames_aliases:[ "Semison" ] ()
  in
  (* Domain state: names not hidden, but record not authenticated *)
  Geneweb_test_utils.Check.bool ~pos:__POS__
    ~msg:
      "Api_privacy_filter.is_hidden returns false for semi-public \
       contemporary person"
    false
    (Plugin_api_lib.Api_privacy_filter.is_hidden semi_vis);
  Geneweb_test_utils.Check.bool ~pos:__POS__
    ~msg:
      "Api_privacy_filter.is_visible returns false for semi-public \
       contemporary person"
    false
    (Plugin_api_lib.Api_privacy_filter.is_visible semi_vis);
  (* Names are revealed because name visibility is permitted (not is_hidden) *)
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname reveals surname when names not hidden"
    "Semi"
    (Plugin_api_lib.Api_privacy_filter.surname semi_vis gen_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name reveals first name when names not hidden"
    "Sam"
    (Plugin_api_lib.Api_privacy_filter.first_name semi_vis gen_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.surname with custom mask reveals surname when names \
       not hidden"
    "Semi"
    (Plugin_api_lib.Api_privacy_filter.surname ~mask:"" semi_vis gen_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.surname_lower reveals lowercased surname when names \
       not hidden"
    "semi"
    (Plugin_api_lib.Api_privacy_filter.surname_lower semi_vis base semi_p);
  Geneweb_test_utils.Check.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.first_name_lower reveals lowercased first name when \
       names not hidden"
    "sam"
    (Plugin_api_lib.Api_privacy_filter.first_name_lower semi_vis base semi_p);
  (* Public name and aliases follow name visibility *)
  Geneweb_test_utils.Check.option Alcotest.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.public_name reveals public name when names not hidden"
    (Some "Lord Sam")
    (Plugin_api_lib.Api_privacy_filter.public_name semi_vis gen_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.aliases reveals aliases when names not hidden"
    [ "Sammy" ]
    (Plugin_api_lib.Api_privacy_filter.aliases semi_vis gen_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.qualifiers reveals qualifiers when names not hidden"
    [ "the Honest" ]
    (Plugin_api_lib.Api_privacy_filter.qualifiers semi_vis gen_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.first_names_aliases reveals first name aliases when \
       names not hidden"
    [ "Samuel" ]
    (Plugin_api_lib.Api_privacy_filter.first_names_aliases semi_vis gen_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.surnames_aliases reveals surname aliases when names \
       not hidden"
    [ "Semison" ]
    (Plugin_api_lib.Api_privacy_filter.surnames_aliases semi_vis gen_p);
  (* Titles strictly require record authentication (is_visible) and are hidden *)
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.titles hides titles when not visible" []
    (Plugin_api_lib.Api_privacy_filter.titles semi_vis visitor_show_names_conf base semi_p);
  Geneweb_test_utils.Check.list Alcotest.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.titles_with_links hides titles with links when not \
       visible"
    []
    (Plugin_api_lib.Api_privacy_filter.titles_with_links semi_vis visitor_show_names_conf base
       semi_p)

let tests =
  [
    Geneweb_test_utils.Check.case "is_hidden" test_is_hidden;
    Geneweb_test_utils.Check.case "aliases" test_aliases;
    Geneweb_test_utils.Check.case "first_names_aliases" test_first_names_aliases;
    Geneweb_test_utils.Check.case "surnames_aliases" test_surnames_aliases;
    Geneweb_test_utils.Check.case "qualifiers" test_qualifiers;
    Geneweb_test_utils.Check.case "public_name" test_public_name;
    Geneweb_test_utils.Check.case "names" test_names;
    Geneweb_test_utils.Check.case "names_lower" test_names_lower;
    Geneweb_test_utils.Check.case "titles" test_titles;
    Geneweb_test_utils.Check.case "contemporary_semi_public" test_contemporary_semi_public;
  ]
