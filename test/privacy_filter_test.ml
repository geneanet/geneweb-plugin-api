open Plugin_api_lib
open Geneweb_test_utils

let setup_db () =
  let db = Mock_db.create () in
  let priv_id =
    Mock_db.add_person db ~first_name:"John" ~surname:"Doe"
      ~access:Def.Private
      ~titles:[ (Def.Tname "Duke", "duke", "of York") ]
      ()
  in
  let pub_id =
    Mock_db.add_person db ~first_name:"Jane" ~surname:"Smith"
      ~access:Def.Public
      ~titles:[ (Def.Tname "Countess", "countess", "of Chester") ]
      ()
  in
  let semi_id =
    Mock_db.add_person db ~first_name:"Sam" ~surname:"Semi"
      ~access:Def.IfTitles
      ~titles:[ (Def.Tname "Baron", "baron", "of Halifax") ]
      ()
  in
  let base = Mock_db.build db in
  ( base,
    Mock_db.poi base priv_id,
    Mock_db.poi base pub_id,
    Mock_db.poi base semi_id )

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
  let priv_vis = Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Api_privacy_filter.evaluate visitor_conf base pub_p in
  let gen_p = make_gen_p ~aliases:[ "Alias1"; "Alias2" ] () in
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.aliases hides aliases for private person from visitor"
    []
    (Api_privacy_filter.aliases priv_vis gen_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.aliases reveals aliases for private person to wizard"
    [ "Alias1"; "Alias2" ]
    (Api_privacy_filter.aliases wiz_vis gen_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.aliases reveals aliases for public person to visitor"
    [ "Alias1"; "Alias2" ]
    (Api_privacy_filter.aliases pub_vis gen_p)

let test_first_names_aliases () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Api_privacy_filter.evaluate visitor_conf base pub_p in
  let gen_p = make_gen_p ~first_names_aliases:[ "Johnny"; "Jack" ] () in
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_names_aliases hides aliases for private person from visitor"
    []
    (Api_privacy_filter.first_names_aliases priv_vis gen_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_names_aliases reveals aliases for private person to wizard"
    [ "Johnny"; "Jack" ]
    (Api_privacy_filter.first_names_aliases wiz_vis gen_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_names_aliases reveals aliases for public person to visitor"
    [ "Johnny"; "Jack" ]
    (Api_privacy_filter.first_names_aliases pub_vis gen_p)

let test_surnames_aliases () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Api_privacy_filter.evaluate visitor_conf base pub_p in
  let gen_p = make_gen_p ~surnames_aliases:[ "Doeman" ] () in
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surnames_aliases hides aliases for private person from visitor"
    []
    (Api_privacy_filter.surnames_aliases priv_vis gen_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surnames_aliases reveals aliases for private person to wizard"
    [ "Doeman" ]
    (Api_privacy_filter.surnames_aliases wiz_vis gen_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surnames_aliases reveals aliases for public person to visitor"
    [ "Doeman" ]
    (Api_privacy_filter.surnames_aliases pub_vis gen_p)

let test_qualifiers () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Api_privacy_filter.evaluate visitor_conf base pub_p in
  let gen_p = make_gen_p ~qualifiers:[ "the Great" ] () in
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.qualifiers hides qualifiers for private person from visitor"
    []
    (Api_privacy_filter.qualifiers priv_vis gen_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.qualifiers reveals qualifiers for private person to wizard"
    [ "the Great" ]
    (Api_privacy_filter.qualifiers wiz_vis gen_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.qualifiers reveals qualifiers for public person to visitor"
    [ "the Great" ]
    (Api_privacy_filter.qualifiers pub_vis gen_p)

let test_public_name () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Api_privacy_filter.evaluate visitor_conf base pub_p in
  let gen_p = make_gen_p ~public_name:"Known Name" () in
  Check.option Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.public_name hides public name for private person from visitor"
    None
    (Api_privacy_filter.public_name priv_vis gen_p);
  Check.option Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.public_name reveals public name for private person to wizard"
    (Some "Known Name")
    (Api_privacy_filter.public_name wiz_vis gen_p);
  Check.option Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.public_name reveals public name for public person to visitor"
    (Some "Known Name")
    (Api_privacy_filter.public_name pub_vis gen_p)

let test_names () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Api_privacy_filter.evaluate visitor_conf base pub_p in
  let gen_p = make_gen_p ~first_name:"John" ~surname:"Doe" () in
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname masks private surname to x for visitor"
    "x"
    (Api_privacy_filter.surname priv_vis gen_p);
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name masks private first name to x for visitor"
    "x"
    (Api_privacy_filter.first_name priv_vis gen_p);
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname masks private surname with custom empty mask"
    ""
    (Api_privacy_filter.surname ~mask:"" priv_vis gen_p);
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name masks private first name with custom empty mask"
    ""
    (Api_privacy_filter.first_name ~mask:"" priv_vis gen_p);
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname reveals private surname to wizard"
    "Doe"
    (Api_privacy_filter.surname wiz_vis gen_p);
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name reveals private first name to wizard"
    "John"
    (Api_privacy_filter.first_name wiz_vis gen_p);
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname reveals public surname to visitor"
    "Doe"
    (Api_privacy_filter.surname pub_vis gen_p);
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name reveals public first name to visitor"
    "John"
    (Api_privacy_filter.first_name pub_vis gen_p)

let test_names_lower () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Api_privacy_filter.evaluate visitor_conf base pub_p in
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname_lower returns empty string for private person to visitor"
    ""
    (Api_privacy_filter.surname_lower priv_vis base priv_p);
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name_lower returns empty string for private person to visitor"
    ""
    (Api_privacy_filter.first_name_lower priv_vis base priv_p);
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname_lower returns lowercased surname for private person to wizard"
    "doe"
    (Api_privacy_filter.surname_lower wiz_vis base priv_p);
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name_lower returns lowercased first name for private person to wizard"
    "john"
    (Api_privacy_filter.first_name_lower wiz_vis base priv_p);
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname_lower returns lowercased surname for public person to visitor"
    "smith"
    (Api_privacy_filter.surname_lower pub_vis base pub_p);
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name_lower returns lowercased first name for public person to visitor"
    "jane"
    (Api_privacy_filter.first_name_lower pub_vis base pub_p)

let test_is_hidden () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Api_privacy_filter.evaluate visitor_conf base pub_p in
  Check.bool ~pos:__POS__
    ~msg:"Api_privacy_filter.is_hidden returns true for private person to visitor"
    true
    (Api_privacy_filter.is_hidden priv_vis);
  Check.bool ~pos:__POS__
    ~msg:"Api_privacy_filter.is_hidden returns false for private person to wizard"
    false
    (Api_privacy_filter.is_hidden wiz_vis);
  Check.bool ~pos:__POS__
    ~msg:"Api_privacy_filter.is_hidden returns false for public person to visitor"
    false
    (Api_privacy_filter.is_hidden pub_vis);
  Check.bool ~pos:__POS__
    ~msg:"Api_privacy_filter.is_visible returns false for private person to visitor"
    false
    (Api_privacy_filter.is_visible priv_vis);
  Check.bool ~pos:__POS__
    ~msg:"Api_privacy_filter.is_visible returns true for private person to wizard"
    true
    (Api_privacy_filter.is_visible wiz_vis);
  Check.bool ~pos:__POS__
    ~msg:"Api_privacy_filter.is_visible returns true for public person to visitor"
    true
    (Api_privacy_filter.is_visible pub_vis)

let test_titles () =
  let base, priv_p, pub_p, _ = setup_db () in
  let priv_vis = Api_privacy_filter.evaluate visitor_conf base priv_p in
  let wiz_vis = Api_privacy_filter.evaluate wizard_conf base priv_p in
  let pub_vis = Api_privacy_filter.evaluate visitor_conf base pub_p in
  (* When not visible (visitor on private person), always returns empty list *)
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.titles returns empty when not visible"
    []
    (Api_privacy_filter.titles priv_vis visitor_conf base priv_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.titles_with_links returns empty when not visible"
    []
    (Api_privacy_filter.titles_with_links priv_vis visitor_conf base priv_p);
  (* When visible to wizard on private person, reveals nobility titles *)
  let expected_priv = [ "duke of York (Duke)" ] in
  let expected_priv_links =
    [ "<a   href=\"?lang=en&m=TT&sm=S&t=duke&p=of+York\">duke of York</a> (Duke)" ]
  in
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.titles returns titles when visible to wizard"
    expected_priv
    (Api_privacy_filter.titles wiz_vis wizard_conf base priv_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.titles_with_links returns titles with links when visible to wizard"
    expected_priv_links
    (Api_privacy_filter.titles_with_links wiz_vis wizard_conf base priv_p);
  (* When visible to visitor on public person, reveals nobility titles *)
  let expected_pub = [ "countess of Chester (Countess)" ] in
  let expected_pub_links =
    [ "<a   href=\"?lang=en&m=TT&sm=S&t=countess&p=of+Chester\">countess of Chester</a> (Countess)" ]
  in
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.titles returns titles for public person when visible"
    expected_pub
    (Api_privacy_filter.titles pub_vis visitor_conf base pub_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.titles_with_links returns titles with links for public person when visible"
    expected_pub_links
    (Api_privacy_filter.titles_with_links pub_vis visitor_conf base pub_p)

let test_contemporary_semi_public () =
  let base, _, _, semi_p = setup_db () in
  let semi_vis =
    Api_privacy_filter.evaluate visitor_show_names_conf base semi_p
  in
  let gen_p =
    make_gen_p ~first_name:"Sam" ~surname:"Semi" ~public_name:"Lord Sam"
      ~aliases:[ "Sammy" ] ~qualifiers:[ "the Honest" ]
      ~first_names_aliases:[ "Samuel" ] ~surnames_aliases:[ "Semison" ] ()
  in
  (* Domain state: names not hidden, but record not authenticated *)
  Check.bool ~pos:__POS__
    ~msg:
      "Api_privacy_filter.is_hidden returns false for semi-public \
       contemporary person"
    false
    (Api_privacy_filter.is_hidden semi_vis);
  Check.bool ~pos:__POS__
    ~msg:
      "Api_privacy_filter.is_visible returns false for semi-public \
       contemporary person"
    false
    (Api_privacy_filter.is_visible semi_vis);
  (* Names are revealed because name visibility is permitted (not is_hidden) *)
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.surname reveals surname when names not hidden"
    "Semi"
    (Api_privacy_filter.surname semi_vis gen_p);
  Check.string ~pos:__POS__
    ~msg:"Api_privacy_filter.first_name reveals first name when names not hidden"
    "Sam"
    (Api_privacy_filter.first_name semi_vis gen_p);
  Check.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.surname with custom mask reveals surname when names \
       not hidden"
    "Semi"
    (Api_privacy_filter.surname ~mask:"" semi_vis gen_p);
  Check.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.surname_lower reveals lowercased surname when names \
       not hidden"
    "semi"
    (Api_privacy_filter.surname_lower semi_vis base semi_p);
  Check.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.first_name_lower reveals lowercased first name when \
       names not hidden"
    "sam"
    (Api_privacy_filter.first_name_lower semi_vis base semi_p);
  (* Public name and aliases follow name visibility *)
  Check.option Alcotest.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.public_name reveals public name when names not hidden"
    (Some "Lord Sam")
    (Api_privacy_filter.public_name semi_vis gen_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.aliases reveals aliases when names not hidden"
    [ "Sammy" ]
    (Api_privacy_filter.aliases semi_vis gen_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.qualifiers reveals qualifiers when names not hidden"
    [ "the Honest" ]
    (Api_privacy_filter.qualifiers semi_vis gen_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.first_names_aliases reveals first name aliases when \
       names not hidden"
    [ "Samuel" ]
    (Api_privacy_filter.first_names_aliases semi_vis gen_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.surnames_aliases reveals surname aliases when names \
       not hidden"
    [ "Semison" ]
    (Api_privacy_filter.surnames_aliases semi_vis gen_p);
  (* Titles strictly require record authentication (is_visible) and are hidden *)
  Check.list Alcotest.string ~pos:__POS__
    ~msg:"Api_privacy_filter.titles hides titles when not visible" []
    (Api_privacy_filter.titles semi_vis visitor_show_names_conf base semi_p);
  Check.list Alcotest.string ~pos:__POS__
    ~msg:
      "Api_privacy_filter.titles_with_links hides titles with links when not \
       visible"
    []
    (Api_privacy_filter.titles_with_links semi_vis visitor_show_names_conf base
       semi_p)

let tests =
  [
    Check.case "is_hidden" test_is_hidden;
    Check.case "aliases" test_aliases;
    Check.case "first_names_aliases" test_first_names_aliases;
    Check.case "surnames_aliases" test_surnames_aliases;
    Check.case "qualifiers" test_qualifiers;
    Check.case "public_name" test_public_name;
    Check.case "names" test_names;
    Check.case "names_lower" test_names_lower;
    Check.case "titles" test_titles;
    Check.case "contemporary_semi_public" test_contemporary_semi_public;
  ]
