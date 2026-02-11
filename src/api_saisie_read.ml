let max_children = 100
let limit_array arr =
  if Array.length arr > max_children then [||] else arr

let limit_list l =
  if List.length l > max_children then [] else l

(**/**) (* Conversion de dates *)

(* Copie de date.ml sans les balises HTML => on devrait créer *)
(* un date_api.ml qu'on utiliserait à la place de date.ml     *)

let short_prec_year_text conf d =
  let prec =
    match d.Date.prec with
    | About | OrYear _ | YearInt _ ->
        (* On utilise le dictionnaire pour être sur *)
        (* que ce soit compréhensible de tous.      *)
        (match Geneweb.Util.transl conf "about (short date)" with
         | "ca" -> "ca "
         | s -> s ^ " ")
    | Maybe -> "? "
    | Before -> "< "
    | After -> "> "
    | _ -> ""
  in
  prec ^ string_of_int d.year

let partial_short_dates_text conf birth_date death_date p =
  match (birth_date, death_date) with
  | (Some (Date.Dgreg (b, _)), Some (Date.Dtext _)) -> short_prec_year_text conf b ^ "-"
  | (Some (Date.Dgreg (b, _)), None) ->
      (* La personne peut être décédée mais ne pas avoir de date. *)
      (match Gwdb.get_death p with
      | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
          short_prec_year_text conf b ^ "-"
      | _ -> short_prec_year_text conf b )
  | (None, Some (Date.Dtext _)) ->
      (match Gwdb.get_death p with
      | Death (_, _) | DeadDontKnowWhen | DeadYoung -> Geneweb.DateDisplay.death_symbol conf
      | _ -> "" )
  | (None, None) ->
      (* La personne peut être décédée mais ne pas avoir de date. *)
      (match Gwdb.get_death p with
      | Death (_, _) | DeadDontKnowWhen | DeadYoung -> Geneweb.DateDisplay.death_symbol conf
      | _ -> "" )
  | (_, _) -> ""

let short_dates_text conf base p =
  if Geneweb.Person.is_visible conf base p then
    let (birth_date, death_date, _) = Gutil.get_birth_death_date p in
    match (birth_date, death_date) with
    | (Some (Date.Dgreg (b, _)), Some (Date.Dgreg (d, _))) ->
      short_prec_year_text conf b ^ "-" ^ short_prec_year_text conf d
    | (Some (Date.Dgreg (b, _)), None) ->
      (* La personne peut être décédée mais ne pas avoir de date. *)
      (match Gwdb.get_death p with
       | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
         short_prec_year_text conf b ^ "-"
       | _ -> short_prec_year_text conf b )
    | (None, Some (Date.Dgreg (d, _))) ->
      Geneweb.DateDisplay.death_symbol conf ^ short_prec_year_text conf d
    | (None, None) ->
      (* La personne peut être décédée mais ne pas avoir de date. *)
      (match Gwdb.get_death p with
       | Death (_, _) | DeadDontKnowWhen | DeadYoung ->
         Geneweb.DateDisplay.death_symbol conf
       | _ -> "" )
    (* On ne peut pas traiter les dates au format texte, mais on *)
    (* affiche tout de même les dates au format Dgreg.           *)
    | (_, _) -> partial_short_dates_text conf birth_date death_date p
  else ""

let string_of_dmy conf d is_long =
  let sy =
    Geneweb.DateDisplay.code_dmy conf d ~with_short_month:(not is_long)
  in
  let sy2 =
    match d.Date.prec with
    | OrYear d2 | YearInt d2 ->
        let d2 = Date.dmy_of_dmy2 d2 in
        Geneweb.DateDisplay.code_dmy conf d2 ~with_short_month:(not is_long)
    | _ ->  ""
  in
  let open Api_util in
  !!(Geneweb.DateDisplay.string_of_prec_dmy
       conf (Adef.safe sy) (Adef.safe sy2) d.Date.prec)

let string_of_dmy_raw (d : Date.dmy) : string =
  let prec =
    match d.Date.prec with
    | About -> "~"
    | Maybe -> "?"
    | Before -> "<"
    | After -> ">"
    | _ -> ""
  in
  let date =
    Printf.sprintf "%d/%d/%d" d.year d.month d.Date.day
  in
  let delta =
    match d.Date.prec with
    | OrYear d2 -> Printf.sprintf "|/%d/%d/%d" d2.year2 d2.month2 d2.Date.day2
    | YearInt d2 -> Printf.sprintf "../%d/%d/%d" d2.year2 d2.month2 d2.Date.day2
    | _ -> ""
  in
  prec ^ "/" ^ date ^ "#" ^ delta

let string_of_date_raw (conf : Geneweb.Config.config) (d : Date.date) : string =
  match d with
  | Date.Dgreg (d, _) -> string_of_dmy_raw d
  | Date.Dtext t -> Geneweb.Util.string_with_macros ~conf ~env:[] t

let string_of_french_dmy conf d =
  Geneweb.DateDisplay.code_french_date conf d.Date.day d.month d.year

let string_of_hebrew_dmy conf d =
  Geneweb.DateDisplay.code_hebrew_date conf d.Date.day d.month d.year

let string_of_islamic_dmy conf d =
  Geneweb.DateDisplay.code_islamic_date conf d.Date.day d.month d.year

let string_of_date_and_conv conf d =
  match d with
  | Date.Dgreg (d, Dgregorian) ->
      let date = string_of_dmy conf d false in
      let date_long = string_of_dmy conf d true in
      let date_conv = date in
      let date_conv_long = date_long in
      (date, date_long, date_conv, date_conv_long, Some `gregorian)
  | Date.Dgreg (d, Djulian) ->
      let d1 = Date.convert ~from:Dgregorian ~to_:Djulian d in
      let date = Geneweb.DateDisplay.code_julian_date conf d1 in
      let date_long =
        let open Api_util in
        !!(Geneweb.DateDisplay.string_of_on_calendar_dmy ~with_gregorian_precisions:false ~calendar:`Julian conf d1)
      in
      let date_conv =
        Adef.as_string @@
          Geneweb.DateDisplay.gregorian_precision conf d ~with_short_month:true
      in
      let date_conv_long =
        let open Api_util in
        !!(Geneweb.DateDisplay.string_of_dmy conf d)
      in
      (date, date_long, date_conv, date_conv_long, Some `julian)
  | Date.Dgreg (d, Dfrench) ->
      let d1 = Date.convert ~from:Dgregorian ~to_:Dfrench d in
      let date = string_of_french_dmy conf d1 in
      let date_long =
        let open Api_util in
        !!(Geneweb.DateDisplay.string_of_on_calendar_dmy ~with_gregorian_precisions:false ~calendar:`French conf d1)
      in
      let date_conv =
        Adef.as_string @@
          Geneweb.DateDisplay.gregorian_precision conf d ~with_short_month:true
      in
      let date_conv_long =
        let open Api_util in
        !!(Geneweb.DateDisplay.string_of_dmy conf d)
      in
      (date, date_long, date_conv, date_conv_long, Some `french)
  | Date.Dgreg (d, Dhebrew) ->
      let d1 = Date.convert ~from:Dgregorian ~to_:Dhebrew d in
      let date = string_of_hebrew_dmy conf d1 in
      let date_long =
        let open Api_util in
        !!(Geneweb.DateDisplay.string_of_on_calendar_dmy ~with_gregorian_precisions:false ~calendar:`Hebrew conf d1)
      in
      let date_conv =
        Adef.as_string @@
          Geneweb.DateDisplay.gregorian_precision conf d ~with_short_month:true
      in
      let date_conv_long =
        let open Api_util in
        !!(Geneweb.DateDisplay.string_of_dmy conf d)
      in
      (date, date_long, date_conv, date_conv_long, Some `hebrew)
  | Date.Dgreg (d, Dislamic) ->
      let d1 = Date.convert ~from:Dgregorian ~to_:Dislamic d in
      let date = string_of_islamic_dmy conf d1 in
      let date_long =
        let open Api_util in
        !!(Geneweb.DateDisplay.string_of_on_calendar_dmy ~with_gregorian_precisions:false ~calendar:`Islamic conf d1)
      in
      let date_conv =
        Adef.as_string @@
          Geneweb.DateDisplay.gregorian_precision conf d ~with_short_month:true
      in
      let date_conv_long =
        let open Api_util in
        !!(Geneweb.DateDisplay.string_of_dmy conf d)
      in
      (date, date_long, date_conv, date_conv_long, Some `islamic)
  | Date.Dtext t -> ("(" ^ Geneweb.Util.string_with_macros ~conf ~env:[] t ^ ")", "", "", "", None)

(**/**) (* Affichage nom/prénom *)

let person_firstname_surname_txt base p =
  if not (Gwdb.is_empty_string (Gwdb.get_public_name p)) then
    let fn = Utf8.normalize @@ Gwdb.sou base (Gwdb.get_public_name p) in
    let sn =
      match Gwdb.get_qualifiers p with
      | s :: _ -> " " ^ Utf8.normalize @@ Gwdb.sou base s
      | _ -> Utf8.normalize @@ Gwdb.sou base (Gwdb.get_surname p)
    in
    (fn, sn)
  else
    let fn = Utf8.normalize @@ Gwdb.sou base (Gwdb.get_first_name p) in
    let sn = Utf8.normalize @@ Gwdb.sou base (Gwdb.get_surname p) in
    let sn =
      match Gwdb.get_qualifiers p with
      | s :: _ -> sn ^ " " ^ Utf8.normalize @@ Gwdb.sou base s
      | _ -> sn
    in
    (fn, sn)

(**/**) (* Fonctions de transformation person <=> piqi person *)

type graph_more_info =
  | Root
  | Siblings
  | Children
  | Ancestor
  | Spouse

let simple_witness_constructor witness_type witness witness_note =
  Api_saisie_read_piqi.Witness_event.({
    witness_type;
    witness;
    witness_note = Utf8.normalize witness_note
  })


let event_to_piqi_event
      (pevt_name : _ Def.gen_pers_event_name option)
      (fevt_name : _ Def.gen_fam_event_name option)
    : Api_saisie_read_piqi.event_type =
  match pevt_name with
  | Some (Def.Epers_Name _) -> `epers_custom
  | Some pevt -> Api_piqi_util.piqi_pevent_name_of_pevent_name pevt
  | None -> match fevt_name with
    | Some (Def.Efam_Name _) -> `efam_custom
    | Some fevt -> Api_piqi_util.piqi_fevent_name_of_fevent_name fevt
    | None -> failwith "event_to_piqi_event"

let pers_to_piqi_person_tree
      (conf : Geneweb.Config.config)
      (base : Gwdb.base)
      (p : Gwdb.person)
      (more_info : graph_more_info)
      (gen : int)
      (max_gen : int)
      (base_prefix : string)
    : Api_saisie_read_piqi.person_tree =
  if Geneweb.Util.is_restricted conf base (Gwdb.get_iper p) then
    {
      Api_saisie_read_piqi.Person_tree.index = Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
      sex = `unknown;
      lastname = "x";
      firstname = "x";
      n = "";
      p = "";
      occ = Int32.of_int 0;
      dates = None;
      image = None;
      sosa = `no_sosa;
      has_more_infos = false;
      baseprefix = "";
      name_is_hidden = Geneweb.NameDisplay.is_hidden conf base p;
      name_is_restricted = Geneweb.NameDisplay.is_restricted conf base p;
    }
  else
    let p_auth = Geneweb.Person.is_visible conf base p in
    let index = Int32.of_string @@ Gwdb.string_of_iper (Gwdb.get_iper p) in
    let sex =
      match Gwdb.get_sex p with
      | Male -> `male
      | Female -> `female
      | Neuter -> `unknown
    in
    let sosa =
      if conf.bname <> Api_util.chop_base_prefix base_prefix then `no_sosa
      else
        let sosa_nb = Geneweb.Sosa_cache.get_sosa_person ~conf ~base ~person:p in
        if Sosa.eq sosa_nb Sosa.zero then `no_sosa
        else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
        else `sosa
    in
    let sn =
      if (Geneweb.Util.is_hide_names conf p) && not p_auth then ""
      else Name.lower (Gwdb.sou base (Gwdb.get_surname p))
    in
    let fn =
      if (Geneweb.Util.is_hide_names conf p) && not p_auth then ""
      else Name.lower (Gwdb.sou base (Gwdb.get_first_name p))
    in
    let occ = Int32.of_int (Gwdb.get_occ p) in
    let (first_name, surname) =
      if not p_auth && (Geneweb.Util.is_hide_names conf p) then ("x", "x")
      else person_firstname_surname_txt base p
    in
    let dates = short_dates_text conf base p in
    let image = Api_util.get_portrait conf base p in
    let has_more_infos =
      match more_info with
      | Root -> false
      | Siblings -> Array.length (Gwdb.get_family p) > 0
      | Children ->
           gen = max_gen - 1 && Array.length (Gwdb.get_family p) > 0
      | Ancestor ->
          let has_parents = Gwdb.get_parents p <> None in
          (gen = max_gen - 1 && has_parents) ||
           (fst (Array.fold_left
                   (fun (children_or_spouses, nb_fam) ifam ->
                     let nb_fam = succ nb_fam in
                     let fam = Gwdb.foi base ifam in
                     let children = limit_array @@ Gwdb.get_children fam in
                     (children_or_spouses || (gen > 1 && Array.length children > 1) || nb_fam > 1,
                      nb_fam))
                   (false, 0) (Gwdb.get_family p)))
      | Spouse ->
          (Gwdb.get_parents p <> None) || Array.length (Gwdb.get_family p) > 1
    in
    {
      Api_saisie_read_piqi.Person_tree.index = index;
      sex = sex;
      lastname = Utf8.normalize surname;
      firstname = Utf8.normalize first_name;
      n = Utf8.normalize sn;
      p = Utf8.normalize fn;
      occ = occ;
      dates = if dates = "" then None else Some (Utf8.normalize dates);
      image = Option.map Utf8.normalize image;
      sosa = sosa;
      has_more_infos = has_more_infos;
      baseprefix = base_prefix;
      name_is_hidden = Geneweb.NameDisplay.is_hidden conf base p;
      name_is_restricted = Geneweb.NameDisplay.is_restricted conf base p;
    }

(* Common functions to build a SimplePerson or a FichePerson. *)
let get_restricted_person () =
  let restricted_person = Api_saisie_read_piqi.default_person () in
  restricted_person.Api_saisie_read_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
  restricted_person.Api_saisie_read_piqi.Person.lastname <- "x";
  restricted_person.Api_saisie_read_piqi.Person.firstname <- "x";
  restricted_person

let get_restricted_fiche_person () =
  let person = get_restricted_person () in
  let fiche = Api_saisie_read_piqi.default_fiche_person () in
  fiche.Api_saisie_read_piqi.Fiche_person.visible_for_visitors <- `visibility_private;
  fiche.Api_saisie_read_piqi.Fiche_person.is_contemporary <- false;
  person.Api_saisie_read_piqi.Person.fiche_person_person <- Some fiche;
  person

let fill_sex p =
      match Gwdb.get_sex p with
      | Male -> `male
      | Female -> `female
      | Neuter -> `unknown

let fill_sosa conf base p =
  let sosa_nb = Geneweb.Sosa_cache.get_sosa_person ~conf ~base ~person:p in
  if Sosa.eq sosa_nb Sosa.zero then `no_sosa
  else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
  else `sosa

let fill_sn conf base p p_auth =
  if (Geneweb.Util.is_hide_names conf p) && not p_auth then ""
  else Name.lower (Gwdb.sou base (Gwdb.get_surname p))

let fill_fn conf base p p_auth =
  if (Geneweb.Util.is_hide_names conf p) && not p_auth then ""
  else Name.lower (Gwdb.sou base (Gwdb.get_first_name p))

let fill_occ p =
  Int32.of_int (Gwdb.get_occ p)

let fill_surname conf p p_auth gen_p =
  if not p_auth && (Geneweb.Util.is_hide_names conf p) then "x" else gen_p.Def.surname

let fill_firstname conf p p_auth gen_p =
  if not p_auth && (Geneweb.Util.is_hide_names conf p) then "x" else gen_p.Def.first_name

let fill_publicname p_auth gen_p =
  let publicname = if not p_auth then "" else gen_p.Def.public_name in
  if publicname = "" then None else Some publicname

let fill_aliases p_auth gen_p =
  if not p_auth then [] else gen_p.Def.aliases

let fill_qualifiers p_auth gen_p =
  if not p_auth then [] else gen_p.Def.qualifiers

let fill_firstname_aliases p_auth gen_p =
  if not p_auth then [] else gen_p.Def.first_names_aliases

let fill_surname_aliases p_auth gen_p =
  if not p_auth then [] else gen_p.Def.surnames_aliases

let pers_to_piqi_simple_person
      (conf : Geneweb.Config.config)
      (base : Gwdb.base)
      (p : Gwdb.person)
      (base_prefix : string)
    : Api_saisie_read_piqi.simple_person =
  if Geneweb.Util.is_restricted conf base (Gwdb.get_iper p) then
    let restricted_person = Api_saisie_read_piqi.default_simple_person() in
    restricted_person.Api_saisie_read_piqi.Simple_person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
    restricted_person.Api_saisie_read_piqi.Simple_person.lastname <- "x";
    restricted_person.Api_saisie_read_piqi.Simple_person.firstname <- "x";
    restricted_person.Api_saisie_read_piqi.Simple_person.visible_for_visitors <- `visibility_private;
    restricted_person
  else
    let p_auth = Geneweb.Person.is_visible conf base p in
    let index = Int32.of_string @@ Gwdb.string_of_iper (Gwdb.get_iper p) in
    let sex =
      match Gwdb.get_sex p with
      | Male -> `male
      | Female -> `female
      | Neuter -> `unknown
    in
    let sosa_nb_num = Geneweb.Sosa_cache.get_sosa_person ~conf ~base ~person:p in
    let sosa =
      if Sosa.eq sosa_nb_num Sosa.zero then `no_sosa
      else if Sosa.eq sosa_nb_num Sosa.one then `sosa_ref
      else `sosa
    in
    let sosa_nb =
        if sosa_nb_num = Sosa.zero
        then None
        else Some (Sosa.to_string sosa_nb_num)
    in
    let sn =
      if (Geneweb.Util.is_hide_names conf p) && not p_auth then ""
      else Name.lower (Gwdb.sou base (Gwdb.get_surname p))
    in
    let fn =
      if (Geneweb.Util.is_hide_names conf p) && not p_auth then ""
      else Name.lower (Gwdb.sou base (Gwdb.get_first_name p))
    in
    let occ = Int32.of_int (Gwdb.get_occ p) in
    let (birth_short, birth_raw, birth_place, death_short, death_raw, death_place) =
      if p_auth then
        let (birth_date, death_date, _) = Gutil.get_birth_death_date p in
        let birth =
          match birth_date with
          | Some d ->
             let open Api_util in
             !!(Geneweb.DateDisplay.string_slash_of_date conf d)
          | None -> ""
        in
        let birth_raw =
          match birth_date with
          | Some d -> (string_of_date_raw conf d)
          | None -> ""
        in
        let birth_place =
          let birth_place = Gwdb.sou base (Gwdb.get_birth_place p) in
          if birth_place <> "" then Geneweb.Util.string_of_place birth_place
          else
            let baptism_place = Gwdb.sou base (Gwdb.get_baptism_place p) in
            Geneweb.Util.string_of_place baptism_place
        in
        let death =
          match death_date with
          | Some d ->
             let open Api_util in
             !!(Geneweb.DateDisplay.string_slash_of_date conf d)
          | None -> ""
        in
        let death_raw =
          match death_date with
          | Some d -> string_of_date_raw conf d
          | None -> ""
        in
        let death_place =
          let death_place = Gwdb.sou base (Gwdb.get_death_place p) in
          if death_place <> "" then Geneweb.Util.string_of_place death_place
          else
            let burial_place = Gwdb.sou base (Gwdb.get_burial_place p) in
            Geneweb.Util.string_of_place burial_place
        in
        let open Api_util in
        (birth, birth_raw, !!birth_place, death, death_raw, !!death_place)
      else ("", "", "", "", "", "")
    in
    let image = Api_util.get_portrait conf base p in
    let has_parent = Gwdb.get_parents p <> None in
    let has_spouse = Array.length (Gwdb.get_family p) >= 1 in
    let has_child =
    (Array.fold_left
        (fun has_children ifam ->
          let fam = Gwdb.foi base ifam in
          let children = limit_array @@ Gwdb.get_children fam in
          (has_children || Array.length children >= 1))
        false (Gwdb.get_family p))
    in
    let gen_p = Geneweb.Util.string_gen_person base (Gwdb.gen_person_of_person p)
    in
    let gen_p = Futil.map_person_ps Fun.id (fun ?format:_ -> Utf8.normalize) gen_p in
    {
      Api_saisie_read_piqi.Simple_person.index = index;
      sex = sex;
      lastname = fill_surname conf p p_auth gen_p;
      firstname = fill_firstname conf p p_auth gen_p;
      n = Utf8.normalize sn;
      p = Utf8.normalize fn;
      occ = occ;
      birth_short_date = if birth_short = "" then None else Some (Utf8.normalize birth_short);
      birth_date_raw = if birth_raw = "" then None else Some (Utf8.normalize birth_raw);
      birth_place = if birth_place = "" then None else Some (Utf8.normalize birth_place);
      death_short_date = if death_short = "" then None else Some (Utf8.normalize death_short);
      death_date_raw = if death_raw = "" then None else Some (Utf8.normalize death_raw);
      death_place = if death_place = "" then None else Some (Utf8.normalize death_place);
      image = Option.map Utf8.normalize image;
      sosa = sosa;
      sosa_nb = sosa_nb;
      visible_for_visitors = Api_util.get_visibility conf base p;
      baseprefix = base_prefix;
      has_parent = has_parent;
      has_spouse = has_spouse;
      has_child = has_child;
      is_contemporary = Geneweb.Person.is_contemporary conf base p;
      name_is_hidden = Geneweb.NameDisplay.is_hidden conf base p;
      name_is_restricted = Geneweb.NameDisplay.is_restricted conf base p;
    }


let fam_to_piqi_family_link
      (conf : Geneweb.Config.config)
      (base : Gwdb.base)
      (ifath : Gwdb.iper)
      (imoth : Gwdb.iper)
      (sp : Gwdb.person)
      (ifam : Gwdb.ifam)
      (fam : Gwdb.family)
      (base_prefix : string)
      (spouse_to_piqi :
         Geneweb.Config.config ->
         Gwdb.base ->
         Gwdb.person ->
         string ->
         'person)
      (witness_to_piqi :
         Geneweb.Config.config ->
         Gwdb.base ->
         Gwdb.person ->
         Def.witness_kind ->
         string ->
         string ->
         'witness)
      (child_to_piqi :
         Geneweb.Config.config ->
         Gwdb.base ->
         Gwdb.person ->
         string ->
         'person)
      (family_link_constructor :
         int32 ->
         'person ->
         string ->
         string ->
         string ->
         string ->
         string ->
         Api_saisie_read_piqi.calendar option ->
         string ->
         string ->
         string ->
         Api_saisie_read_piqi.marriage_type ->
         Api_saisie_read_piqi.divorce_type ->
         string ->
         string ->
         string ->
         string ->
         string ->
         Api_saisie_read_piqi.calendar option ->
         'witness list ->
         string ->
         string ->
         'person list ->
         'family)
    : 'family =
  let spouse = spouse_to_piqi conf base sp base_prefix in
  let p_auth = true in
  let m_auth = true in
  let gen_f = Geneweb.Util.string_gen_family base (Gwdb.gen_family_of_family fam) in
  let gen_f = Futil.map_family_ps Fun.id Fun.id (fun ?format:_ -> Utf8.normalize) gen_f in
  let index = Int32.of_string @@ Gwdb.string_of_ifam gen_f.fam_index in
  let (marriage_date, marriage_date_long, marriage_date_conv, marriage_date_conv_long, marriage_cal, marriage_date_raw) =
    match (m_auth, Date.od_of_cdate gen_f.marriage) with
    | (true, Some d) ->
      let (marriage_date, marriage_date_long, marriage_date_conv, marriage_date_conv_long, marriage_cal) = string_of_date_and_conv conf d in
      (marriage_date, marriage_date_long, marriage_date_conv, marriage_date_conv_long, marriage_cal, string_of_date_raw conf d)
    | _ -> ("", "", "", "", None, "")
  in
  let marriage_date_text =
    let open Api_util in
    !!(Geneweb.Perso.get_marriage_date_text conf fam p_auth)
  in
  let marriage_place =
    if m_auth then
      let open Api_util in
      !!(Geneweb.Util.string_of_place gen_f.marriage_place)
    else ""
  in
  let marriage_src =
    if m_auth then
      let open Api_util in
      !!(Geneweb.Notes.source conf base gen_f.marriage_src)
    else ""
  in
  let marriage_type =
    match gen_f.relation with
    | Married -> `married
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
  in
  let (divorce_type, divorce_date, divorce_date_long, divorce_date_conv, divorce_date_conv_long, divorce_cal, divorce_date_raw) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, "", "", "", "", None, "")
    | Divorced cod ->
        (match Date.od_of_cdate cod with
         | Some d when m_auth ->
             let (divorce_date, divorce_date_long, divorce_date_conv, divorce_date_conv_long, divorce_cal) =
               string_of_date_and_conv conf d
             in
             (`divorced, divorce_date, divorce_date_long, divorce_date_conv, divorce_date_conv_long, divorce_cal, string_of_date_raw conf d)
         | _ -> (`divorced, "", "", "", "", None, ""))
    | Separated -> (`separated, "", "", "", "", None, "")
  in
  let witnesses =
    Ext_array.to_list_map
      (fun (ip, wkind, wnote) ->
         let p = Gwdb.poi base ip in
         let wnote = Utf8.normalize @@ Gwdb.sou base wnote in
         witness_to_piqi conf base p wkind wnote base_prefix
      ) (Geneweb.Perso.get_marriage_witnesses_and_notes fam)
  in
  let notes =
    if m_auth && not conf.Geneweb.Config.no_note
    then
      let open Api_util in
      !!(Geneweb.Notes.note conf base [] gen_f.comment)
    else ""
  in
  let fsources =
    if m_auth
    then
      let open Api_util in
      !!(Geneweb.Notes.source conf base gen_f.fsources)
    else ""
  in
  let children =
    List.map
      (fun (p, base_prefix) -> child_to_piqi conf base p base_prefix)
      (!Geneweb.GWPARAM_ITL.get_children_of_parents base base_prefix ifam ifath imoth |> limit_list)
  in
  family_link_constructor index spouse marriage_date marriage_date_long marriage_date_raw marriage_date_conv marriage_date_conv_long
    marriage_cal marriage_date_text marriage_place marriage_src marriage_type divorce_type divorce_date divorce_date_long divorce_date_raw divorce_date_conv
    divorce_date_conv_long divorce_cal witnesses notes fsources children

let fill_events
      ?(page : Api_util.Page.t option)
      (conf : Geneweb.Config.config)
      (base : Gwdb.base)
      (p : Gwdb.person)
      (base_prefix : string)
      (p_auth : bool)
      (pers_to_piqi :
         Geneweb.Config.config ->
         Gwdb.base ->
         Gwdb.person ->
         string ->
         'person)
      (witness_constructor :
         Api_saisie_read_piqi.witness_type ->
         'person ->
         string ->
         'witness)
      (event_constructor :
         string ->
         Api_saisie_read_piqi.event_type ->
         string ->
         string ->
         string ->
         string ->
         string ->
         Api_saisie_read_piqi.calendar option ->
         string ->
         string ->
         string ->
         'person option ->
         'witness list ->
         'event)
    : 'event Api_util.Paginated_data.t =
  let extract_page =
    Option.fold
      ~none:Api_util.Paginated_data.all
      ~some:Api_util.Paginated_data.extract
      page
  in
  if p_auth then
    let make_event evt =
      let name = Geneweb.Event.get_name evt in
      let date = Geneweb.Event.get_date evt in
      let place = Geneweb.Event.get_place evt in
      let note = Geneweb.Event.get_note evt in
      let src = Geneweb.Event.get_src evt in
      let w = Geneweb.Event.get_witnesses_and_notes evt in
      let isp = Geneweb.Event.get_spouse_iper evt in
      let (name, type_) =
        match name with
        | Geneweb.Event.Pevent name ->
           let open Api_util in
           ( !!(Geneweb.Util.string_of_pevent_name conf base name)
           , event_to_piqi_event (Some name) None)
        | Geneweb.Event.Fevent name ->
           let open Api_util in
           ( !!(Geneweb.Util.string_of_fevent_name conf base name)
           , event_to_piqi_event None (Some name) )
      in
      let (date, date_long, date_conv, date_conv_long, date_cal, date_raw) =
        match Date.od_of_cdate date with
        | Some d ->
           let (date, date_long, date_conv, date_conv_long, date_cal) = string_of_date_and_conv conf d in
           (date, date_long, date_conv, date_conv_long, date_cal, string_of_date_raw conf d)
        | _ -> ("", "", "", "", None, "")
      in
      let place =
        let open Api_util in
        !!(Geneweb.Util.string_of_place (Gwdb.sou base place))
      in
      let note =
        if not conf.Geneweb.Config.no_note
        then
          let open Api_util in
          !!(Geneweb.Notes.person_note ~keep_newlines:true conf base p (Gwdb.sou base note))
        else ""
      in
      let src =
        let open Api_util in
        !!(Geneweb.Notes.source conf base (Gwdb.sou base src))
      in
      let spouse =
        Option.map (fun ip -> pers_to_piqi conf base (Gwdb.poi base ip) base_prefix) isp
      in
      let witnesses =
        Ext_array.to_list_map
          (fun (ip, wk, wnote) ->
            let witness_type = Api_util.piqi_of_witness_kind wk in
            let witness = Gwdb.poi base ip in
            let witness = pers_to_piqi conf base witness base_prefix in
            let wnote = Utf8.normalize @@ Gwdb.sou base wnote in
            witness_constructor witness_type witness wnote
          )
          w
      in
      event_constructor name type_ date date_long date_raw date_conv date_conv_long date_cal place note src spouse witnesses
    in
    let events = Geneweb.Event.sorted_events conf base p in
    Api_util.Paginated_data.map make_event (extract_page events)
  else Api_util.Paginated_data.all []


let fill_events_if_is_main_person conf base p base_prefix p_auth is_main_person pers_to_piqi witness_constructor event_constructor =
  if is_main_person then
    (fill_events conf base p base_prefix p_auth pers_to_piqi witness_constructor event_constructor).elements
  else []

let get_related_piqi conf base p base_prefix pers_to_piqi relation_person_constructor =
    List.map
      (fun (p, rp) ->
        let p = pers_to_piqi conf base p base_prefix in
        let r_type =
          match rp.Def.r_type with
          | Adoption -> `rchild_adoption
          | Recognition -> `rchild_recognition
          | CandidateParent -> `rchild_candidate_parent
          | GodParent -> `rchild_god_parent
          | FosterParent -> `rchild_foster_parent
        in
        relation_person_constructor r_type p
        )
      (Geneweb.Relation.get_others_related conf base p)

let get_family_piqi base conf ifam p base_prefix spouse_to_piqi witnesses_to_piqi child_to_piqi family_constructor =
  let fam = Gwdb.foi base ifam in
  let sp = Gwdb.poi base (Gutil.spouse (Gwdb.get_iper p) fam) in
  let spouse = spouse_to_piqi conf base sp base_prefix in
  let ifath = Gwdb.get_father fam in
  let imoth = Gwdb.get_mother fam in
  let p_auth = Geneweb.Person.is_visible conf base p in
  let m_auth =
    Geneweb.Person.is_visible conf base (Geneweb.Util.pget conf base ifath) &&
    Geneweb.Person.is_visible conf base (Geneweb.Util.pget conf base imoth)
  in
  let gen_f = Geneweb.Util.string_gen_family base (Gwdb.gen_family_of_family fam) in
  let gen_f = Futil.map_family_ps Fun.id Fun.id (fun ?format:_ -> Utf8.normalize) gen_f in
  let index = Int32.of_string @@ Gwdb.string_of_ifam gen_f.fam_index in
  let (marriage_date, marriage_date_long, marriage_date_conv, marriage_date_conv_long, marriage_cal, marriage_date_raw) =
    match (m_auth, Date.od_of_cdate gen_f.marriage) with
    | (true, Some d) ->
      let (marriage_date, marriage_date_long, marriage_date_conv, marriage_date_conv_long, marriage_cal) = string_of_date_and_conv conf d in
      (marriage_date, marriage_date_long, marriage_date_conv, marriage_date_conv_long, marriage_cal, string_of_date_raw conf d)
    | _ -> ("", "", "", "", None, "")
  in
  let marriage_date_text =
    let open Api_util in
    !!(Geneweb.Perso.get_marriage_date_text conf fam p_auth)
  in
  let marriage_place =
    if m_auth then
      let open Api_util in
      !!(Geneweb.Util.string_of_place gen_f.marriage_place)
    else ""
  in
  let marriage_src =
    if m_auth then
      let open Api_util in
      !!(Geneweb.Notes.source conf base gen_f.marriage_src)
    else ""
  in
  let marriage_type =
    match gen_f.relation with
    | Married -> `married
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
  in
  let (divorce_type, divorce_date, divorce_date_long, divorce_date_conv, divorce_date_conv_long, divorce_cal, divorce_date_raw) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, "", "", "", "", None, "")
    | Divorced cod ->
        (match Date.od_of_cdate cod with
         | Some d when m_auth ->
             let (divorce_date, divorce_date_long, divorce_date_conv, divorce_date_conv_long, divorce_cal) =
               string_of_date_and_conv conf d
             in
             (`divorced, divorce_date, divorce_date_long, divorce_date_conv, divorce_date_conv_long, divorce_cal, string_of_date_raw conf d)
         | _ -> (`divorced, "", "", "", "", None, ""))
    | Separated -> (`separated, "", "", "", "", None, "")
  in
  let witnesses =
    Ext_array.to_list_map
      (fun (ip, wkind, wnote) ->
         let p = Gwdb.poi base ip in
         let wnote = Utf8.normalize @@ Gwdb.sou base wnote in
         witnesses_to_piqi conf base p wkind wnote base_prefix
      )
      (Geneweb.Perso.get_marriage_witnesses_and_notes fam)
  in
  let notes =
    if m_auth && not conf.no_note
    then
      let open Api_util in
      !!(Geneweb.Notes.note conf base [] gen_f.comment )
    else ""
  in
  let fsources =
    if m_auth
    then
      let open Api_util in
      !!(Geneweb.Notes.source conf base gen_f.fsources)
    else ""
  in
  let children =
    let children_array = limit_array @@ Gwdb.get_children fam in
    Ext_array.to_list_map
      (fun ip -> child_to_piqi conf base (Gwdb.poi base ip) base_prefix)
      children_array
  in
  (* lien inter arbre *)
  let children_link =
    List.fold_right begin fun (_, _, children) acc ->
      List.fold_right begin fun ((p, _), baseprefix, can_merge) acc ->
        if can_merge then acc
        else child_to_piqi conf base p baseprefix :: acc
      end children acc
    end (!Geneweb.GWPARAM_ITL.get_children' conf base (Gwdb.get_iper p) fam (Gwdb.get_iper sp)) []
  in
  let children = children @ children_link in
    family_constructor index spouse marriage_date marriage_date_long marriage_date_raw marriage_date_conv marriage_date_conv_long
    marriage_cal marriage_date_text marriage_place marriage_src marriage_type divorce_type divorce_date divorce_date_long divorce_date_raw divorce_date_conv
    divorce_date_conv_long divorce_cal witnesses notes fsources children

let get_families_piqi base conf p base_prefix spouse_to_piqi witnesses_to_piqi child_to_piqi family_constructor =
  let families =
    Ext_array.to_list_map
      (fun ifam ->
         get_family_piqi base conf ifam p base_prefix spouse_to_piqi witnesses_to_piqi child_to_piqi family_constructor
      )
      (Gwdb.get_family p)
  in
  (* lien inter arbre *)
  let families_link =
    List.fold_right begin fun (ifam, fam, (ifath, imoth, isp), baseprefix, can_merge ) acc ->
      if can_merge then acc
      else
        fam_to_piqi_family_link conf base ifath imoth isp ifam fam baseprefix spouse_to_piqi witnesses_to_piqi child_to_piqi family_constructor :: acc
    end (!Geneweb.GWPARAM_ITL.get_families conf base p) []
  in
    families @ families_link

let get_rparents_piqi base conf base_prefix gen_p pers_to_piqi relation_person_constructor =
    List.fold_left
      (fun rl rp ->
        let r_type =
          match rp.Def.r_type with
          | Adoption -> `rparent_adoption
          | Recognition -> `rparent_recognition
          | CandidateParent -> `rparent_candidate_parent
          | GodParent -> `rparent_god_parent
          | FosterParent -> `rparent_foster_parent
        in
        let to_relation_person conf base ip =
          let p = Gwdb.poi base ip in
          let p = pers_to_piqi conf base p base_prefix in
          relation_person_constructor r_type p
        in
        let rl =
          match rp.r_fath with
          | Some ip ->
              let p = to_relation_person conf base ip in
              p :: rl
          | None -> rl
        in
        match rp.r_moth with
        | Some ip -> to_relation_person conf base ip :: rl
        | None -> rl)
      [] gen_p.Def.rparents

let get_events_witnesses ?page conf base p base_prefix p_auth pers_to_piqi event_witness_constructor =
    let events_witnesses =
      let extract_page =
        Option.fold
          ~none:Api_util.Paginated_data.all
          ~some:Api_util.Paginated_data.extract
          page
      in
      let events_witnesses = Geneweb.Relation.get_event_witnessed conf base p in
      extract_page events_witnesses
    in
    let make_event_witness (witness, wk, wnote, evt) =
      let wk = Geneweb.Util.string_of_witness_kind conf (Gwdb.get_sex p) wk in
      let event_name =
        match Geneweb.Event.get_name evt with
        | Geneweb.Event.Pevent name ->
           if p_auth then
             let open Api_util in
             !!(Geneweb.Util.string_of_pevent_name conf base name)
           else  ""
        | Geneweb.Event.Fevent name ->
           if p_auth then
             let open Api_util in
             !!(Geneweb.Util.string_of_fevent_name conf base name)
           else  ""
      in
      let s =
        match Date.cdate_to_dmy_opt (Geneweb.Event.get_date evt) with
        | None ->
           let open Api_util in
           Printf.sprintf "(%s) : %s"
             !!(wk) event_name
        | Some dmy ->
           let open Api_util in
           Printf.sprintf "%s (%s) : %s"
             (Geneweb.DateDisplay.year_text dmy) !!(wk) event_name
      in
      let event_witness_type = Geneweb.Util.translate_eval (Geneweb.Util.transl_a_of_b conf s "" "") in
      let husband = pers_to_piqi conf base witness base_prefix in
      let wife =
        match Geneweb.Event.get_spouse_iper evt with
        | Some isp ->
           let sp = Gwdb.poi base isp in
           Some (pers_to_piqi conf base sp base_prefix )
        | None -> None
      in
      event_witness_constructor event_witness_type husband wife wnote
    in
    Api_util.Paginated_data.map make_event_witness events_witnesses

let fill_birth_place p_auth gen_p =
  if p_auth then
    let open Api_util in
    !!(Geneweb.Util.string_of_place gen_p.Def.birth_place)
  else ""

let fill_baptism_place p_auth gen_p =
  if p_auth then
    let open Api_util in
    !!(Geneweb.Util.string_of_place gen_p.Def.baptism_place)
  else ""

let fill_death_place p_auth gen_p =
  if p_auth then
    let open Api_util in
    !!(Geneweb.Util.string_of_place gen_p.Def.death_place)
  else ""

let fill_birth_src conf base p_auth gen_p =
  if p_auth then
    let open Api_util in
    !!(Geneweb.Notes.source conf base gen_p.Def.birth_src)
  else ""

let fill_burial_src conf base p_auth gen_p =
  if p_auth then
    let open Api_util in
    !!(Geneweb.Notes.source conf base gen_p.Def.burial_src)
  else ""

let fill_death_src conf base p_auth gen_p =
  if p_auth then
    let open Api_util in
    !!(Geneweb.Notes.source conf base gen_p.Def.death_src)
  else ""

let fill_baptism_src conf base p_auth gen_p =
  if p_auth then
    let open Api_util in
    !!(Geneweb.Notes.source conf base gen_p.Def.baptism_src)
  else ""

let fill_burial_place p_auth gen_p =
  if p_auth then
    let open Api_util in
    !!(Geneweb.Util.string_of_place gen_p.Def.burial_place)
  else ""

let fill_death conf p_auth gen_p =
  match (p_auth, gen_p.Def.death) with
      | (true, NotDead) -> (`not_dead, "", "", None)
      | (true, Death (_, cd)) ->
          let d = Date.date_of_cdate cd in
          let (death, _, death_conv, _, death_cal) = string_of_date_and_conv conf d in
          (`dead, death, death_conv, death_cal)
      | (true, DeadYoung) -> (`dead_young, "", "", None)
      | (true, DeadDontKnowWhen) -> (`dead_dont_know_when, "", "", None)
      | (true, DontKnowIfDead) -> (`dont_know_if_dead, "", "", None)
      | (true, OfCourseDead) -> (`of_course_dead, "", "", None)
      | _ -> (`dont_know_if_dead, "", "", None)

let fill_birth conf p_auth gen_p =
  match (p_auth, Date.od_of_cdate gen_p.Def.birth) with
      | (true, Some d) -> string_of_date_and_conv conf d
      | _ -> ("", "", "", "", None)

let fill_baptism conf p_auth gen_p =
  match (p_auth, Date.od_of_cdate gen_p.Def.baptism) with
      | (true, Some d) -> string_of_date_and_conv conf d
      | _ -> ("", "", "", "", None)

let fill_burial conf p_auth gen_p =
  match (p_auth, gen_p.Def.burial) with
      | (true, Buried cod) | (true, Cremated cod) ->
          (match Date.od_of_cdate cod with
          | Some d -> string_of_date_and_conv conf d
          | _ -> ("", "", "", "", None))
      | _ -> ("", "", "", "", None)

let fill_occupation conf base p_auth gen_p =
  if p_auth
  then
    let open Api_util in
    !!(Geneweb.Notes.source conf base gen_p.Def.occupation)
  else ""

let fill_index conf p p_auth =
  if not p_auth && (Geneweb.Util.is_hide_names conf p)
  then
    Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper
  else
    Int32.of_string @@ Gwdb.string_of_iper (Gwdb.get_iper p)

let fill_sources conf base p_auth gen_p is_main_person =
  if p_auth && is_main_person
  then
    let open Api_util in
    !!(Geneweb.Notes.source conf base gen_p.Def.psources)
  else ""

let fill_parents conf base p base_prefix =
  match Gwdb.get_parents p with
  | Some ifam ->
    let cpl = Gwdb.foi base ifam in
    let ifath = Gwdb.get_father cpl in
    let imoth = Gwdb.get_mother cpl in
    let father =
      if ifath = Gwdb.dummy_iper then None
      else
        let father = Gwdb.poi base ifath in
        Some (pers_to_piqi_simple_person conf base father base_prefix)
    in
    let mother =
      if imoth = Gwdb.dummy_iper then None
      else
        let mother = Gwdb.poi base imoth in
        Some (pers_to_piqi_simple_person conf base mother base_prefix)
    in
    (father, mother)
  | None ->
    (* lien inter arbre *)
    let ip = Gwdb.get_iper p in
    let aux fn =
      match fn conf base base_prefix ip with
      | Some ((p, _), base_prefix) -> Some (pers_to_piqi_simple_person conf base p base_prefix)
      | None -> None
    in
    (aux !Geneweb.GWPARAM_ITL.get_father, aux !Geneweb.GWPARAM_ITL.get_mother)

let fill_fiche_parents conf base p base_prefix nb_asc nb_asc_max with_parent_families pers_to_piqi_person simple_graph_info no_event =
  if nb_asc_max > nb_asc
  then
    match Gwdb.get_parents p with
    | Some ifam ->
      let cpl = Gwdb.foi base ifam in
      let ifath = Gwdb.get_father cpl in
      let imoth = Gwdb.get_mother cpl in
      let father =
        if ifath = Gwdb.dummy_iper then None
        else
          let father = Gwdb.poi base ifath in
          if with_parent_families then
            Some (pers_to_piqi_person conf base father base_prefix false (nb_asc+1) nb_asc_max 0 2 true simple_graph_info no_event)
          else
            Some (pers_to_piqi_person conf base father base_prefix false (nb_asc+1) nb_asc_max 0 0 false simple_graph_info no_event)
      in
      let mother =
        if imoth = Gwdb.dummy_iper then None
        else
          let mother = Gwdb.poi base imoth in
          if with_parent_families then
            Some (pers_to_piqi_person conf base mother base_prefix false (nb_asc+1) nb_asc_max 0 2 true simple_graph_info no_event)
          else
            Some (pers_to_piqi_person conf base mother base_prefix false (nb_asc+1) nb_asc_max 0 0 false simple_graph_info no_event)
      in
      (father, mother)
    | None ->
      (* lien inter arbre *)
      let ip = Gwdb.get_iper p in
      let aux fn =
        match fn conf base base_prefix ip with
        | Some ((p, _), baseprefix) ->
          if with_parent_families
          then Some (pers_to_piqi_person conf base p baseprefix false (nb_asc + 1) nb_asc_max 0 1 true simple_graph_info no_event)
          else Some (pers_to_piqi_person conf base p baseprefix false (nb_asc + 1) nb_asc_max 0 0 false simple_graph_info no_event)
        | None -> None
      in
      (aux !Geneweb.GWPARAM_ITL.get_father, aux !Geneweb.GWPARAM_ITL.get_mother)
  else
    (None, None)

let get_event_constructor name type_ date date_long date_raw date_conv date_conv_long date_cal place note src spouse witnesses =
      {
        Api_saisie_read_piqi.Event.name = Utf8.normalize name;
        type_ = type_;
        date = if date = "" then None else Some (Utf8.normalize date);
        date_long = if date_long = "" then None else Some (Utf8.normalize date_long);
        date_raw = if date_raw = "" then None else Some (Utf8.normalize date_raw);
        date_conv = if date_conv = "" then None else Some (Utf8.normalize date_conv);
        date_conv_long = if date_conv_long = "" then None else Some (Utf8.normalize date_conv_long);
        date_cal = date_cal;
        place = if place = "" then None else Some (Utf8.normalize place);
        reason = None;
        note = if note = "" then None else Some (Utf8.normalize note);
        src = if src= "" then None else Some (Utf8.normalize src);
        spouse = spouse;
        witnesses = witnesses;
      }

let fiche_event_constructor name type_ date date_long date_raw date_conv date_conv_long date_cal place note src spouse witnesses =
  {
      Api_saisie_read_piqi.Fiche_event.name = Utf8.normalize name;
      type_ = type_;
      date = if date = "" then None else Some (Utf8.normalize date);
      date_long = if date_long = "" then None else Some (Utf8.normalize date_long);
      date_raw = if date_raw = "" then None else Some (Utf8.normalize date_raw);
      date_conv = if date_conv = "" then None else Some (Utf8.normalize date_conv);
      date_conv_long = if date_conv_long = "" then None else Some (Utf8.normalize date_conv_long);
      date_cal = date_cal;
      place = if place = "" then None else Some (Utf8.normalize place);
      reason = None;
      note = if note = "" then None else Some (Utf8.normalize note);
      src = if src= "" then None else Some (Utf8.normalize src);
      spouse = spouse;
      witnesses = witnesses;
  }

let fiche_witness_constructor witness_type witness witness_note =
  Api_saisie_read_piqi.Witness_fiche_event.({
    witness_type;
    witness;
    witness_note = Utf8.normalize witness_note
  })

let simple_event_witness_constructor event_witness_type husband wife witness_note =
      Api_saisie_read_piqi.Event_witness.({
        event_witness_type = event_witness_type;
        husband = husband;
        wife = wife;
        witness_note = Utf8.normalize witness_note
      })

let fiche_event_witness_constructor event_witness_type husband wife witness_note =
  Api_saisie_read_piqi.Event_fiche_witness.({
    event_witness_type = event_witness_type;
    husband = husband;
    wife = wife;
    witness_note = Utf8.normalize witness_note
  })

let fill_notes conf base p p_auth is_main_person gen_p =
  if p_auth && not conf.Geneweb.Config.no_note && is_main_person
  then
    let open Api_util in
    !!(Geneweb.Notes.person_note ~keep_newlines:true conf base p gen_p.Def.notes)
  else ""

let simple_relation_person_constructor r_type p =
  {
    Api_saisie_read_piqi.Relation_person.r_type = r_type;
    person = p;
  }

let fiche_relation_person_constructor r_type p =
  {
    Api_saisie_read_piqi.Relation_fiche_person.r_type = r_type;
    person = p;
  }

let fill_families conf base p =
  let base_prefix = conf.Geneweb.Config.command in
  let spouse_to_piqi conf base p base_prefix =
      pers_to_piqi_simple_person conf base p base_prefix
  in
  let witnesses_to_piqi conf base p wkind wnote base_prefix =
    let p = pers_to_piqi_simple_person conf base p base_prefix in
    let wkind = Api_util.piqi_of_witness_kind wkind in
    simple_witness_constructor wkind p wnote
  in
  let child_to_piqi conf base p base_prefix =
      pers_to_piqi_simple_person conf base p base_prefix
  in
  let family_constructor index spouse marriage_date marriage_date_long marriage_date_raw marriage_date_conv marriage_date_conv_long marriage_cal
       marriage_date_text marriage_place marriage_src marriage_type divorce_type divorce_date divorce_date_long divorce_date_raw divorce_date_conv
       divorce_date_conv_long divorce_cal witnesses notes fsources children =
    {
      Api_saisie_read_piqi.Family.index = index;
      spouse = spouse;
      marriage_date = if marriage_date = "" then None else Some (Utf8.normalize marriage_date);
      marriage_date_long = if marriage_date_long = "" then None else Some (Utf8.normalize marriage_date_long);
      marriage_date_raw = if marriage_date_raw = "" then None else Some (Utf8.normalize marriage_date_raw);
      marriage_date_conv =
        if marriage_date_conv = "" then None else Some (Utf8.normalize marriage_date_conv);
      marriage_date_conv_long =
        if marriage_date_conv_long = "" then None else Some (Utf8.normalize marriage_date_conv_long);
      marriage_date_cal = marriage_cal;
      marriage_date_text = if marriage_date_text = "" then None else Some (Utf8.normalize marriage_date_text);
      marriage_place = if marriage_place = "" then None else Some (Utf8.normalize marriage_place);
      marriage_src = if marriage_src = "" then None else Some (Utf8.normalize marriage_src);
      marriage_type = marriage_type;
      divorce_type = divorce_type;
      divorce_date = if divorce_date = "" then None else Some (Utf8.normalize divorce_date);
      divorce_date_long = if divorce_date_long = "" then None else Some (Utf8.normalize divorce_date_long);
      divorce_date_raw = if divorce_date_raw = "" then None else Some (Utf8.normalize divorce_date_raw);
      divorce_date_conv =
        if divorce_date_conv = "" then None else Some (Utf8.normalize divorce_date_conv);
      divorce_date_conv_long =
        if divorce_date_conv_long = "" then None else Some (Utf8.normalize divorce_date_conv_long);
      divorce_date_cal = divorce_cal;
      witnesses = witnesses;
      notes = if notes = "" then None else Some (Utf8.normalize notes);
      fsources = if fsources = "" then None else Some (Utf8.normalize fsources);
      children = children;
    }
  in
  get_families_piqi base conf p base_prefix spouse_to_piqi witnesses_to_piqi child_to_piqi family_constructor

let fill_fiche_families conf base p base_prefix nb_asc nb_asc_max nb_desc nb_desc_max pers_to_piqi_person simple_graph_info no_event =
  let include_families = nb_desc_max > nb_desc && nb_asc <= nb_asc_max in
  if include_families
  then
    let spouse_to_piqi conf base p base_prefix =
      pers_to_piqi_person conf base p base_prefix false 0 1 0 0 false simple_graph_info no_event
    in
    let witnesses_to_piqi conf base p wkind wnote base_prefix =
      let p = if not simple_graph_info then
          pers_to_piqi_person conf base p base_prefix false 0 1 0 0 false simple_graph_info no_event
        else Api_saisie_read_piqi.default_person ()
      in
      let wkind = Api_util.piqi_of_witness_kind wkind in
      fiche_witness_constructor wkind p wnote
    in
    let child_to_piqi conf base p base_prefix =
      pers_to_piqi_person conf base p base_prefix false 0 0 (nb_desc+1) nb_desc_max false simple_graph_info no_event
    in
    let family_constructor index spouse marriage_date marriage_date_long marriage_date_raw marriage_date_conv marriage_date_conv_long marriage_cal
    marriage_date_text marriage_place marriage_src marriage_type divorce_type divorce_date divorce_date_long divorce_date_raw divorce_date_conv
    divorce_date_conv_long divorce_cal witnesses notes fsources children
      =
      {
        Api_saisie_read_piqi.Fiche_family.index = index;
        spouse = spouse;
        marriage_date = if marriage_date = "" then None else Some (Utf8.normalize marriage_date);
        marriage_date_long = if marriage_date_long = "" then None else Some (Utf8.normalize marriage_date_long);
        marriage_date_raw = if marriage_date_raw = "" then None else Some (Utf8.normalize marriage_date_raw);
        marriage_date_conv =
          if marriage_date_conv = "" then None else Some (Utf8.normalize marriage_date_conv);
        marriage_date_conv_long =
          if marriage_date_conv_long = "" then None else Some (Utf8.normalize marriage_date_conv_long);
        marriage_date_cal = marriage_cal;
        marriage_date_text = if marriage_date_text = "" then None else Some (Utf8.normalize marriage_date_text);
        marriage_place = if marriage_place = "" then None else Some (Utf8.normalize marriage_place);
        marriage_src = if marriage_src = "" then None else Some (Utf8.normalize marriage_src);
        marriage_type = marriage_type;
        divorce_type = divorce_type;
        divorce_date = if divorce_date = "" then None else Some (Utf8.normalize divorce_date);
        divorce_date_long = if divorce_date_long = "" then None else Some (Utf8.normalize divorce_date_long);
        divorce_date_raw = if divorce_date_raw = "" then None else Some (Utf8.normalize divorce_date_raw);
        divorce_date_conv =
          if divorce_date_conv = "" then None else Some (Utf8.normalize divorce_date_conv);
        divorce_date_conv_long =
          if divorce_date_conv_long = "" then None else Some (Utf8.normalize divorce_date_conv_long);
        divorce_date_cal = divorce_cal;
        witnesses = if not simple_graph_info then witnesses else [];
        notes = if notes = "" || simple_graph_info then None else Some (Utf8.normalize notes);
        fsources = if fsources = "" || simple_graph_info then None else Some (Utf8.normalize fsources);
        children = children;
      }
    in
      get_families_piqi base conf p base_prefix spouse_to_piqi witnesses_to_piqi child_to_piqi family_constructor
  else []

let has_sources p_auth psources birth_src baptism_src death_src burial_src =
  if not p_auth then false
    else if psources <> "" then true
    else if
      p_auth &&
      (birth_src <> "" || baptism_src <> "" ||
       death_src <> "" || burial_src <> "")
    then true
  else false

let fill_titles conf base p =
  List.map
    (fun x ->
      let open Api_util in
      !!(Geneweb.Perso.string_of_title ~safe:true ~link:false conf base (Adef.safe "") p x))
    (Geneweb.Perso.nobility_titles_list conf base p)

let transform_empty_string_to_None string =
  if string = "" then None else Some string

let fill_birth_date_raw conf p_auth gen_p =
  match (p_auth, Date.od_of_cdate gen_p.Def.birth) with
    | (true, Some d) -> string_of_date_raw conf d
    | _ -> ""

let fill_baptism_date_raw conf p_auth gen_p =
  match (p_auth, Date.od_of_cdate gen_p.Def.baptism) with
    | (true, Some d) -> string_of_date_raw conf d
    | _ -> ""

let fill_death_date_raw conf p_auth gen_p =
  match (p_auth, gen_p.Def.death) with
      | (true, Death (_, cd)) ->
          let d = Date.date_of_cdate cd in
          string_of_date_raw conf d
      | _ -> ""

let fill_burial_date_raw_if_is_main_person conf p_auth gen_p is_main_person =
  if is_main_person then
    match (p_auth, gen_p.Def.burial) with
    | (true, Buried cod) | (true, Cremated cod) ->
        (match Date.od_of_cdate cod with
        | Some d -> string_of_date_raw conf d
        | _ -> "")
    | _ -> ""
  else
    ""

let fill_birth_text conf p p_auth =
  let open Api_util in
  !!(Geneweb.Perso.get_birth_text conf p p_auth)

let fill_baptism_text conf p p_auth =
  let open Api_util in
  !!(Geneweb.Perso.get_baptism_text conf p p_auth)

let fill_death_text conf p p_auth =
  let open Api_util in
  !!(Geneweb.Perso.get_death_text conf p p_auth)

let fill_burial_text conf p p_auth =
  let open Api_util in
  !!(Geneweb.Perso.get_burial_text conf p p_auth)

let fill_cremation_text conf p p_auth =
  let open Api_util in
  !!(Geneweb.Perso.get_cremation_text conf p p_auth)

let fill_burial_type p_auth gen_p =
  if p_auth then
  match (gen_p.Def.burial) with
    | Buried _ -> `buried
    | Cremated _ -> `cremated
    | _ -> `dont_know
  else `dont_know

let fill_titles_with_links conf base p =
  List.map
    (fun x ->
      let open Api_util in
      !!(Geneweb.Perso.string_of_title ~link:true conf base (Adef.safe "") p x))
    (Geneweb.Perso.nobility_titles_list conf base p)

let has_history_if_is_main_person conf base p p_auth is_main_person =
  if is_main_person then
    Geneweb.Perso.has_history conf base p p_auth
  else false

let has_duplication_if_is_main_person conf base p is_main_person =
  (* Les doublons ne sont pas testés pour les LIA. *)
  if is_main_person then
      Geneweb.Perso.has_possible_duplications conf base p
  else
      false

let fill_linked_page_if_is_main_person conf base p is_main_person =
  if is_main_person then
    let open Api_util in
    ( !!(Geneweb.Perso.get_linked_page conf base p "BIBLIO")
    , !!(Geneweb.Perso.get_linked_page conf base p "BNOTE")
    , !!(Geneweb.Perso.get_linked_page conf base p "DEATH")
    , !!(Geneweb.Perso.get_linked_page conf base p "HEAD")
    , !!(Geneweb.Perso.get_linked_page conf base p "OCCU")
    )
  else
    ("", "", "", "", "")

let pers_to_piqi_person
      ?events_limit
      ?events_witnesses_limit
      (conf : Geneweb.Config.config)
      (base : Gwdb.base)
      (p : Gwdb.person)
      (base_prefix : string)
      (is_main_person : bool)
    : Api_saisie_read_piqi.person =
  if Geneweb.Util.is_restricted conf base (Gwdb.get_iper p) then
    get_restricted_person ()
  else
    let p_auth = Geneweb.Person.is_visible conf base p in
    let gen_p = Geneweb.Util.string_gen_person base (Gwdb.gen_person_of_person p) in
    let gen_p = Futil.map_person_ps Fun.id (fun ?format:_ -> Utf8.normalize) gen_p in
    let (baptism_date, _, baptism_date_conv, _, baptism_cal) = fill_baptism conf p_auth gen_p in
    let (birth_date, _, birth_date_conv, _, birth_cal) = fill_birth conf p_auth gen_p in
    let (burial_date, _, burial_date_conv, _,burial_cal) = fill_burial conf p_auth gen_p in
    let (death_type, death_date, death_date_conv, death_cal) = fill_death conf p_auth gen_p in

    let (father, mother) = fill_parents conf base p base_prefix in

    let psources = fill_sources conf base p_auth gen_p is_main_person in
    let birth_src = fill_birth_src conf base p_auth gen_p in
    let baptism_src = fill_baptism_src conf base p_auth gen_p in
    let death_src = fill_death_src conf base p_auth gen_p in
    let burial_src = fill_burial_src conf base p_auth gen_p in
    let has_sources = has_sources p_auth psources birth_src baptism_src death_src burial_src in
    let events =
      fill_events
        ?page:
        (Option.map
           (fun element_count -> Api_util.Page.first ~element_count)
           events_limit)
        conf
        base
        p
        base_prefix
        p_auth
        pers_to_piqi_simple_person
        simple_witness_constructor
        get_event_constructor
    in
    let events_witnesses =
      get_events_witnesses
        ?page:
        (Option.map
           (fun element_count -> Api_util.Page.first ~element_count)
           events_witnesses_limit)
        conf
        base
        p
        base_prefix
        p_auth
        pers_to_piqi_simple_person
        simple_event_witness_constructor
    in

    {
      Api_saisie_read_piqi.Person.type_ = `simple;
      index = fill_index conf p p_auth;
      sex = fill_sex p;
      lastname = fill_surname conf p p_auth gen_p;
      firstname = fill_firstname conf p p_auth gen_p;
      n = fill_sn conf base p p_auth;
      p = fill_fn conf base p p_auth;
      occ = fill_occ p;
      public_name = fill_publicname p_auth gen_p;
      aliases = fill_aliases p_auth gen_p;
      qualifiers = fill_qualifiers p_auth gen_p;
      firstname_aliases = fill_firstname_aliases p_auth gen_p;
      surname_aliases = fill_surname_aliases p_auth gen_p;
      image = Api_util.get_portrait conf base p;
      birth_date = transform_empty_string_to_None birth_date;
      birth_date_conv = transform_empty_string_to_None birth_date_conv;
      birth_date_cal = birth_cal;
      birth_place = transform_empty_string_to_None (fill_birth_place p_auth gen_p);
      birth_src = transform_empty_string_to_None (fill_birth_src conf base p_auth gen_p);
      baptism_date = transform_empty_string_to_None baptism_date;
      baptism_date_conv = transform_empty_string_to_None baptism_date_conv;
      baptism_date_cal = baptism_cal;
      baptism_place = transform_empty_string_to_None (fill_baptism_place p_auth gen_p);
      baptism_src = transform_empty_string_to_None baptism_src;
      death_date = transform_empty_string_to_None death_date;
      death_date_conv = transform_empty_string_to_None death_date_conv;
      death_date_cal = death_cal;
      death_place = transform_empty_string_to_None (fill_death_place p_auth gen_p);
      death_src = transform_empty_string_to_None death_src;
      death_type = death_type;
      burial_date = transform_empty_string_to_None burial_date;
      burial_date_conv = transform_empty_string_to_None burial_date_conv;
      burial_date_cal = burial_cal;
      burial_place = transform_empty_string_to_None (fill_burial_place p_auth gen_p);
      burial_src = transform_empty_string_to_None burial_src;
      occupation = transform_empty_string_to_None (fill_occupation conf base p_auth gen_p);
      notes = transform_empty_string_to_None (fill_notes conf base p p_auth is_main_person gen_p);
      psources = transform_empty_string_to_None psources;
      has_sources = has_sources;
      titles = fill_titles conf base p;
      related = get_related_piqi conf base p base_prefix pers_to_piqi_simple_person simple_relation_person_constructor;
      rparents = get_rparents_piqi base conf base_prefix gen_p pers_to_piqi_simple_person simple_relation_person_constructor;
      father = father;
      mother = mother;
      families = fill_families conf base p;
      sosa = fill_sosa conf base p;
      events = Api_util.Paginated_data.Piqi.to_personal_events events;
      events_witnesses =
        Api_util.Paginated_data.Piqi.to_witnessed_events events_witnesses;
      baseprefix = base_prefix;
      fiche_person_person = None;
      is_contemporary = Geneweb.Person.is_contemporary conf base p;
      name_is_hidden = Geneweb.NameDisplay.is_hidden conf base p;
      name_is_restricted = Geneweb.NameDisplay.is_restricted conf base p;
    }

let fill_ref_if_is_main_person conf base is_main_person =
  if is_main_person then
    match Geneweb.Util.find_sosa_ref conf base with
      | Some ref -> ( Some (Int32.of_string @@ Gwdb.string_of_iper (Gwdb.get_iper ref))
                    , Some (pers_to_piqi_person conf base ref conf.Geneweb.Config.command false) )
      | None -> (None, None)
  else
    (None, None)

let rec pers_to_piqi_fiche_person
          (conf : Geneweb.Config.config)
          (base : Gwdb.base)
          (p : Gwdb.person)
          (base_prefix : string)
          (is_main_person : bool)
          (nb_asc : int)
          (nb_asc_max : int)
          (nb_desc : int)
          (nb_desc_max : int)
          (with_parent_families : bool)
          (simple_graph_info : bool)
          (no_event : bool)
        : Api_saisie_read_piqi.person =
  (* Generates a fiche person by default. *)
  let piqi_fiche_person = Api_saisie_read_piqi.default_fiche_person() in
  (* If the access is restricted, returns the person with default fields. *)
  if Geneweb.Util.is_restricted conf base (Gwdb.get_iper p) then
    get_restricted_fiche_person ()
  else
    begin
      let p_auth = Geneweb.Person.is_visible conf base p in
      let gen_p = Geneweb.Util.string_gen_person base (Gwdb.gen_person_of_person p) in
      let gen_p = Futil.map_person_ps Fun.id (fun ?format:_ -> Utf8.normalize) gen_p in
      (* Sources only returned for the main person. *)
      let psources = if is_main_person then fill_sources conf base p_auth gen_p is_main_person else "" in
      let birth_src = if is_main_person then fill_birth_src conf base p_auth gen_p else "" in
      let baptism_src = if is_main_person then fill_baptism_src conf base p_auth gen_p else "" in
      let death_src = if is_main_person then fill_death_src conf base p_auth gen_p else "" in
      let burial_src = if is_main_person then fill_burial_src conf base p_auth gen_p else "" in
      let has_sources = if is_main_person then has_sources p_auth psources birth_src baptism_src death_src burial_src else false in
      let (death_type, death_date, death_date_conv, death_cal) = fill_death conf p_auth gen_p in
      (* Linked links (family book). *)
      let (linked_page_biblio, linked_page_bnote, linked_page_death, linked_page_head, linked_page_occu) = if not simple_graph_info then fill_linked_page_if_is_main_person conf base p is_main_person else ("", "", "", "", "") in
      let pers_to_piqi_fiche_person_only conf base p base_prefix =
        pers_to_piqi_fiche_person conf base p base_prefix false 0 0 0 0 false simple_graph_info no_event
      in
      let sosa_nb = Geneweb.Sosa_cache.get_sosa_person ~conf ~base ~person:p in
      let (fiche_father, fiche_mother) = if is_main_person || not simple_graph_info then fill_fiche_parents conf base p base_prefix nb_asc nb_asc_max with_parent_families pers_to_piqi_fiche_person simple_graph_info no_event else (None, None) in
      let (father, mother) = if with_parent_families then fill_parents conf base p base_prefix else (None, None) in
      (* Returns simple person attributes only when nb of desc is 0. *)
      let return_simple_attributes = (nb_desc_max == 0) in
      let (ref_index, ref_person) = fill_ref_if_is_main_person conf base is_main_person in
      let piqi_fiche_person =
        (* Fields shared by all the members of the family. *)
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.birth_date_raw <- transform_empty_string_to_None (fill_birth_date_raw conf p_auth gen_p);
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.birth_text <- transform_empty_string_to_None (fill_birth_text conf p p_auth);
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.burial_date_raw <- transform_empty_string_to_None (fill_burial_date_raw_if_is_main_person conf p_auth gen_p is_main_person);
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.burial_text <- transform_empty_string_to_None (fill_burial_text conf p p_auth);
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.burial_type <- fill_burial_type p_auth gen_p;
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.cremation_text <- transform_empty_string_to_None (fill_cremation_text conf p p_auth);
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.death_date_raw <- transform_empty_string_to_None (fill_death_date_raw conf p_auth gen_p);
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.death_text <- transform_empty_string_to_None (fill_death_text conf p p_auth);
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.titles_links <- if not simple_graph_info then fill_titles_with_links conf base p else [];
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.sosa_nb <- if sosa_nb = Sosa.zero then None else Some (Sosa.to_string sosa_nb);
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.father <- fiche_father;
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.mother <- fiche_mother;
        if is_main_person || not simple_graph_info then
          piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.families <- fill_fiche_families conf base p base_prefix nb_asc nb_asc_max nb_desc nb_desc_max pers_to_piqi_fiche_person simple_graph_info no_event;

        (* Fields only filled for the main person. *)
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.baptism_date_raw <- if is_main_person then transform_empty_string_to_None (fill_baptism_date_raw conf p_auth gen_p) else None;
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.baptism_text <- if is_main_person then transform_empty_string_to_None (fill_baptism_text conf p p_auth) else None;
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.has_possible_duplications <- has_duplication_if_is_main_person conf base p is_main_person;
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.ref_index <- ref_index;
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.ref_person <- ref_person;
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.has_history <- has_history_if_is_main_person conf base p p_auth is_main_person;
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.linked_page_biblio <- linked_page_biblio;
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.linked_page_bnote <- linked_page_bnote;
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.linked_page_death <- linked_page_death;
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.linked_page_head <- linked_page_head;
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.linked_page_occu <- linked_page_occu;
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.visible_for_visitors <- Api_util.get_visibility conf base p;
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.related <- if is_main_person && not simple_graph_info then get_related_piqi conf base p base_prefix pers_to_piqi_fiche_person_only fiche_relation_person_constructor else [];
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.rparents <- if is_main_person && not simple_graph_info then get_rparents_piqi base conf base_prefix gen_p pers_to_piqi_fiche_person_only fiche_relation_person_constructor else [];
        if not no_event then
          piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.events_witnesses <- if is_main_person then (get_events_witnesses conf base p base_prefix p_auth pers_to_piqi_fiche_person_only fiche_event_witness_constructor).elements else [];
        if not no_event then
          piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.events <- fill_events_if_is_main_person conf base p base_prefix p_auth is_main_person pers_to_piqi_fiche_person_only fiche_witness_constructor fiche_event_constructor;
        piqi_fiche_person.Api_saisie_read_piqi.Fiche_person.is_contemporary <- Geneweb.Person.is_contemporary conf base p;
        piqi_fiche_person
      in
      let events =
        if return_simple_attributes && not no_event then
          fill_events
            conf
            base
            p
            base_prefix
            p_auth
            pers_to_piqi_simple_person
            simple_witness_constructor
            get_event_constructor
        else Api_util.Paginated_data.all []
      in
      let events_witnesses =
        if return_simple_attributes && not no_event then
          get_events_witnesses
            conf
            base
            p
            base_prefix
            p_auth
            pers_to_piqi_simple_person
            simple_event_witness_constructor
        else Api_util.Paginated_data.all []
      in
      {
        Api_saisie_read_piqi.Person.type_ = `fiche;
        fiche_person_person = Some piqi_fiche_person;
        n = fill_sn conf base p p_auth;
        p = fill_fn conf base p p_auth;
        occ = fill_occ p;

        baptism_src = transform_empty_string_to_None baptism_src;
        birth_place = transform_empty_string_to_None (fill_birth_place p_auth gen_p);
        birth_src = transform_empty_string_to_None birth_src;
        burial_place = transform_empty_string_to_None (fill_burial_place p_auth gen_p);
        burial_src = transform_empty_string_to_None burial_src;
        death_date = transform_empty_string_to_None death_date;
        death_date_conv = transform_empty_string_to_None death_date_conv;
        death_date_cal = death_cal;
        death_place = transform_empty_string_to_None (fill_death_place p_auth gen_p);
        death_src = transform_empty_string_to_None death_src;
        death_type = death_type;
        index = fill_index conf p p_auth;
        image = Api_util.get_portrait conf base p;
        firstname = fill_firstname conf p p_auth gen_p;
        lastname = fill_surname conf p p_auth gen_p;
        qualifiers = if (not simple_graph_info) || is_main_person then fill_qualifiers p_auth gen_p else [];
        occupation = transform_empty_string_to_None (fill_occupation conf base p_auth gen_p);
        sex = fill_sex p;
        public_name = fill_publicname p_auth gen_p;

        (* Fields only filled for the main person. *)
        baptism_place = if is_main_person then transform_empty_string_to_None (fill_baptism_place p_auth gen_p) else None;
        aliases = if is_main_person && not simple_graph_info then fill_aliases p_auth gen_p else [];
        firstname_aliases = if is_main_person && not simple_graph_info then fill_firstname_aliases p_auth gen_p else [];
        has_sources = has_sources;
        notes = if is_main_person && not simple_graph_info then transform_empty_string_to_None (fill_notes conf base p p_auth is_main_person gen_p) else None;
        psources = if is_main_person && not simple_graph_info then transform_empty_string_to_None psources else None;
        sosa = if is_main_person then fill_sosa conf base p else `no_sosa;
        surname_aliases = if is_main_person && not simple_graph_info then fill_surname_aliases p_auth gen_p else [];

        (* These fields should not be set because Fiche Person fields are better. *)
        baptism_date = None;
        baptism_date_conv = None;
        baptism_date_cal = None;
        birth_date = None;
        birth_date_conv = None;
        birth_date_cal = None;
        burial_date = None;
        burial_date_conv = None;
        burial_date_cal = None;
        events = Api_util.Paginated_data.Piqi.to_personal_events events;
        events_witnesses =
          Api_util.Paginated_data.Piqi.to_witnessed_events events_witnesses;
        families = if return_simple_attributes && not simple_graph_info then fill_families conf base p else [];
        father = if return_simple_attributes then father else None;
        mother = if return_simple_attributes then mother else None;
        titles = if not simple_graph_info then fill_titles conf base p else [];
        related = if return_simple_attributes then get_related_piqi conf base p base_prefix pers_to_piqi_simple_person simple_relation_person_constructor else [];
        rparents = if return_simple_attributes then get_rparents_piqi base conf base_prefix gen_p pers_to_piqi_simple_person simple_relation_person_constructor else [];
        baseprefix = base_prefix;
        is_contemporary = Geneweb.Person.is_contemporary conf base p;
        name_is_hidden = Geneweb.NameDisplay.is_hidden conf base p;
        name_is_restricted = Geneweb.NameDisplay.is_restricted conf base p;
      }
    end

let print_person_tree conf base =
  let params = Api_util.get_params conf Api_saisie_read_piqi_ext.parse_index_person in
  let ip = Gwdb.iper_of_string @@ Int32.to_string params.Api_saisie_read_piqi.Index_person.index in
  if Gwdb.iper_exists base ip then
  (* Construction de la base avec calcul des sosas           *)
  (* Si iz présent, on prend iz comme souche pour le calcul  *)
  (* Sinon on prend la souche de l'arbre                     *)
  let conf =
    match params.Api_saisie_read_piqi.Index_person.indexz with
    | Some n -> Api_util.set_sosa_ref conf (Int32.to_string n |> Gwdb.iper_of_string)
    | None -> conf
  in
  let p = Gwdb.poi base ip in
  (* cache lien inter arbre *)
  let () = !Geneweb.GWPARAM_ITL.init_cache conf base ip 1 1 1 in
  let pers_piqi =
    let events_limit =
      Option.map
        Int32.to_int params.Api_saisie_read_piqi.Index_person.events_limit
    in
    let events_witnesses_limit =
      Option.map
        Int32.to_int
        params.Api_saisie_read_piqi.Index_person.events_witnesses_limit
    in
    pers_to_piqi_person
      ?events_limit
      ?events_witnesses_limit
      conf
      base
      p
      conf.Geneweb.Config.command
      true
  in
  let data = Api_saisie_read_piqi_ext.gen_person pers_piqi in
  Api_util.print_result conf data
  else begin
    Geneweb.Output.status conf Def.Not_Found ;
    Geneweb.Output.print_sstring conf ""
  end

type search_index_type =  Sosa | Key | Surname | FirstName | ApproxKey | PartialKey;;
let search_index
      (conf : Geneweb.Config.config)
      (base : Gwdb.base)
      (an : string)
      (search_order : search_index_type list)
    : Gwdb.iper option =
  let rec loop l =
    match l with
    | Sosa::le ->
      begin match Sosa.of_string an with
      | Some sosa ->
        begin match Geneweb.SearchName.search_by_sosa ~conf ~base ~sosa with
          | None ->  loop le
          | Some p -> Some (Gwdb.get_iper p)
        end
      | None -> loop le
      end
    | Key::le ->
      begin match Geneweb.SearchName.search_by_key conf base an with
        | None ->  loop le
        | Some p -> Some (Gwdb.get_iper p)
      end
    | Surname::le ->
      if Geneweb.Search_name_display.sn_search_result_is_empty
          (Geneweb.Search_name_display.search_surname
             ~exact:false
             conf
             base
             an)
      then loop le
      else None
    | FirstName::le ->
      if Geneweb.Search_name_display.fn_search_result_is_empty
          (Geneweb.Search_name_display.search_first_name
             ~exact:false
             conf
             base
             an)
      then loop le
      else None
    | ApproxKey::le ->
      begin match Geneweb.SearchName.search_approx_key conf base an with
        | [] ->  loop le
        | [p] -> Some (Gwdb.get_iper p)
        | _ -> None
      end
    | PartialKey::le ->
      begin match Geneweb.SearchName.search_partial_key conf base an with
        | [] ->  loop le
        | [p] -> Some (Gwdb.get_iper p)
        | _ -> None
      end
    | _ -> None
  in
  loop search_order

let print_result_fiche_person conf base ip nb_asc_max nb_desc_max simple_graph_info no_event =
  if Gwdb.iper_exists base ip then begin
    let p = Gwdb.poi base ip in
    (* cache lien inter arbre *)
    let () = !Geneweb.GWPARAM_ITL.init_cache conf base ip 1 1 1 in
    let pers_piqi = pers_to_piqi_fiche_person conf base p conf.Geneweb.Config.command true 0 nb_asc_max 0 nb_desc_max true simple_graph_info no_event in
    let data = Api_saisie_read_piqi_ext.gen_person pers_piqi in
    Api_util.print_result conf data
  end else begin
    Geneweb.Output.status conf Def.Not_Found ;
    Geneweb.Output.print_sstring conf ""
  end

let print_from_identifier_person
      (conf : Geneweb.Config.config)
      (base : Gwdb.base)
      (print_result_from_ip :
         Geneweb.Config.config -> Gwdb.base -> Gwdb.iper -> unit)
      (identifier_person : Api_saisie_read_piqi.identifier_person)
    : unit =
  match identifier_person.Api_saisie_read_piqi.Identifier_person.index with
  | Some index ->
    (* Traite l'index *)
    let ip = Gwdb.iper_of_string @@ Int32.to_string index in
    if identifier_person.Api_saisie_read_piqi.Identifier_person.track_visit = Some true
    then Geneweb.Util.record_visited conf ip;
    print_result_from_ip conf base ip
  | None ->
    match (identifier_person.Api_saisie_read_piqi.Identifier_person.oc) with
    | (Some oc) ->
      begin
        match ( identifier_person.Api_saisie_read_piqi.Identifier_person.p
              , identifier_person.Api_saisie_read_piqi.Identifier_person.n) with
        | (Some fn, Some sn) ->
          (* Retourne une personne en fonction de son npoc *)
          begin
            match Gwdb.person_of_key base fn sn (Int32.to_int oc) with
            | Some ip ->
              let p = Gwdb.poi base ip in
              if Geneweb.Person.is_empty p || ((Geneweb.Util.is_hide_names conf p) && not(Geneweb.Person.is_visible conf base p)) then
                Api_util.print_error conf `not_found ""
              else
                (if identifier_person.Api_saisie_read_piqi.Identifier_person.track_visit
                    = Some true
                 then Geneweb.Util.record_visited conf ip;
                 print_result_from_ip conf base ip)
            | None ->
              Api_util.print_error conf `not_found ""
          end
        | _ -> Api_util.print_error conf `bad_request ""
        end
    | None ->
      (* Fait une recherche par mots-clé *)
      let (fn, sn) =
        match ( identifier_person.Api_saisie_read_piqi.Identifier_person.p
              , identifier_person.Api_saisie_read_piqi.Identifier_person.n) with
        | (Some fn, Some sn) -> (fn, sn)
        | (None, Some sn) -> ("", sn)
        | (Some fn, None) -> (fn, "")
        | _ -> Api_util.print_error conf `bad_request ""
      in
      let (an, order) =
        if fn = "" then
          (sn, [ Sosa; Key; Surname; ApproxKey; PartialKey ])
        else if sn = "" then
          (fn, [ FirstName ])
        else
          (fn ^ " " ^ sn, [ Key; ApproxKey; PartialKey ])
      in match search_index conf base an order with
      | Some ip ->
        if identifier_person.Api_saisie_read_piqi.Identifier_person.track_visit = Some true
        then Geneweb.Util.record_visited conf ip;
        print_result_from_ip conf base ip
      | None -> Api_util.print_error conf `not_found ""

let print_fiche_person conf base =
  let fiche_parameters = Api_util.get_params conf Api_saisie_read_piqi_ext.parse_fiche_parameters in
  let identifier_person = fiche_parameters.Api_saisie_read_piqi.Fiche_parameters.identifier_person in
  let print_result_from_ip conf base ip =
      let nb_asc_max =
        match fiche_parameters.Api_saisie_read_piqi.Fiche_parameters.nb_asc_max with
        | Some n -> Int32.to_int n
        | None -> 1 (* Add grand-parents. *)
      in
      let nb_desc_max =
        match fiche_parameters.Api_saisie_read_piqi.Fiche_parameters.nb_desc_max with
        | Some n -> Int32.to_int n
        | None -> 0
      in
      let simple_graph_info =
        match fiche_parameters.Api_saisie_read_piqi.Fiche_parameters.simple_graph_info with
        | Some b -> b
        | None -> false
      in
      let no_event =
        match fiche_parameters.Api_saisie_read_piqi.Fiche_parameters.no_event with
        | Some b -> b
        | None -> false
      in
      print_result_fiche_person conf base ip nb_asc_max nb_desc_max simple_graph_info no_event
  in
  print_from_identifier_person conf base print_result_from_ip identifier_person


(**/**)

let hash_id x = Int64.of_int (Hashtbl.hash x)

let create_edge factor_from baseprefix_from p_from factor_to baseprefix_to p_to =
  let from_node = hash_id (baseprefix_from, Gwdb.get_iper p_from, factor_from) in
  let to_node = hash_id (baseprefix_to, Gwdb.get_iper p_to, factor_to) in
  Api_saisie_read_piqi.Edge.{ from_node ; to_node }

let create_node conf base max_gen ifam p gen more_info base_prefix factor =
  let id = hash_id (base_prefix, Gwdb.get_iper p, factor) in
  let p = pers_to_piqi_person_tree conf base p more_info gen max_gen base_prefix in
  { Api_saisie_read_piqi.Node.id = id
  ; person = p
  ; ifam
  }

let factor ht x =
  match Hashtbl.find_opt ht x with
  | Some i ->
    let i = i + 1 in
    Hashtbl.replace ht x i;
    i
  | None ->
    Hashtbl.add ht x 1;
    1

(* Graphe d'ascendance *)
let build_graph_asc conf base p max_gen =
  let create_node = create_node conf base max_gen None in
  let ht = Hashtbl.create 0 in
  let nodes = ref [] in
  let edges = ref [] in
  let rec loop = function
    | [] -> ()
    | (p, gen) :: l ->
      if gen >= max_gen then loop l
      else match Gwdb.get_parents p with
        | Some ifam ->
          let p_factor = Option.value (Hashtbl.find_opt ht (Gwdb.get_iper p)) ~default:1 in
          let cpl = Gwdb.foi base ifam in
          let fath = Gwdb.poi base (Gwdb.get_father cpl) in
          let moth = Gwdb.poi base (Gwdb.get_mother cpl) in
          let fath_factor = factor ht (Gwdb.get_iper fath) in
          let moth_factor = factor ht (Gwdb.get_iper moth) in
          nodes := create_node fath gen Ancestor conf.Geneweb.Config.command fath_factor :: !nodes;
          nodes := create_node moth gen Ancestor conf.Geneweb.Config.command moth_factor :: !nodes;
          edges := create_edge p_factor conf.Geneweb.Config.command p fath_factor conf.Geneweb.Config.command fath :: !edges;
          edges := create_edge p_factor conf.Geneweb.Config.command p moth_factor conf.Geneweb.Config.command moth :: !edges;
          loop ((fath, gen + 1) :: (moth, gen + 1) :: l)
        | None ->
          (* lien inter arbre *)
          let ip = Gwdb.get_iper p in
          let () = !Geneweb.GWPARAM_ITL.init_cache conf base ip (max_gen - gen) 0 0 in
          let () =
            let ht = Hashtbl.create 0 in
            let rec loop_parents l =
              match l with
              | [] -> ()
              | (base_prefix, p, gen) :: l ->
                if gen >= max_gen then loop_parents l
                else
                  let ip = Gwdb.get_iper p in
                  let p_factor = Option.value (Hashtbl.find_opt ht (base_prefix, Gwdb.get_iper p)) ~default:1 in
                  match !Geneweb.GWPARAM_ITL.get_father conf base base_prefix ip
                      , !Geneweb.GWPARAM_ITL.get_mother conf base base_prefix ip with
                  | (Some ((fath, _), bpf), Some ((moth, _), bpm)) ->
                    let fath_factor = factor ht (bpf, Gwdb.get_iper fath) in
                    let moth_factor = factor ht (bpm, Gwdb.get_iper moth) in
                    nodes := create_node fath gen Ancestor bpf fath_factor :: !nodes;
                    nodes := create_node moth gen Ancestor bpm moth_factor :: !nodes;
                    edges := create_edge p_factor base_prefix p fath_factor bpf fath :: !edges;
                    edges := create_edge p_factor base_prefix p moth_factor bpm moth :: !edges;
                    let l = (bpf, fath, gen + 1) :: (bpm, moth, gen + 1) :: l in
                    loop_parents l
                  | _ -> loop_parents l
            in
            loop_parents [ (conf.Geneweb.Config.command, p, gen) ]
          in
          loop l
  in
  nodes := create_node p 1 Root conf.Geneweb.Config.command 1 :: !nodes;
  loop [(p, 1)];
  (* On retourne la liste pour avoir les noeuds dans l'ordre *)
  (* la référence, suivi du père suivi, puis de la mère ...  *)
  (List.rev !nodes, List.rev !edges)

(* Graphe de descendance *)
let build_graph_desc conf base p max_gen =
  let ht = Hashtbl.create 0 in
  let create_node ifam =
    create_node conf base max_gen (Some (Int64.of_string (Gwdb.string_of_ifam ifam)))
  in
  let nodes = ref [] in
  let edges = ref [] in
  let rec loop = function
    | [] -> ()
    | (p, gen) :: l ->
      if gen >= max_gen then loop l
      else
        let p_factor = Option.value (Hashtbl.find_opt ht (Gwdb.get_iper p)) ~default:1 in
        let ifam = Gwdb.get_family p in
        let l =
          Array.fold_left (fun acc ifam  ->
              let fam = Gwdb.foi base ifam in
              let sp = Gwdb.poi base (Gutil.spouse (Gwdb.get_iper p) fam) in
              let sp_factor = factor ht (Gwdb.get_iper sp) in
              let children = Ext_array.to_list_map (Gwdb.poi base) (limit_array @@ Gwdb.get_children fam) in
              nodes := create_node ifam sp gen Spouse conf.Geneweb.Config.command sp_factor :: !nodes;
              edges := create_edge p_factor conf.Geneweb.Config.command p sp_factor conf.Geneweb.Config.command sp :: !edges;
              if gen <> max_gen then begin
                List.iter begin fun c ->
                  let c_factor = factor ht (Gwdb.get_iper c) in
                  nodes := create_node ifam c gen Children conf.Geneweb.Config.command c_factor :: !nodes;
                  edges := create_edge p_factor conf.Geneweb.Config.command p c_factor conf.Geneweb.Config.command c :: !edges;
                  edges := create_edge sp_factor conf.Geneweb.Config.command sp c_factor conf.Geneweb.Config.command c :: !edges
                end children;
                let child_local =
                  List.fold_left (fun acc c -> (c, gen + 1) :: acc) acc children
                in
                (* lien inter arbre *)
                let () = !Geneweb.GWPARAM_ITL.init_cache conf base (Gwdb.get_iper p) 1 1 (max_gen - gen) in
                let () =
                  let ht = Hashtbl.create 0 in
                  let rec loop_child = function
                    | [] -> ()
                    | (base_prefix, p, gen) :: l ->
                      if gen >= max_gen then loop_child l
                      else
                        let p_factor = Option.value (Hashtbl.find_opt ht (base_prefix, Gwdb.get_iper p)) ~default:1 in
                        let l =
                          List.fold_left begin fun acc (fam_bp, (_, _, isp), children) ->
                            let sp_factor = factor ht (fam_bp, isp) in
                            List.fold_left begin fun acc ((c, _), baseprefix, can_merge) ->
                              if can_merge then acc
                              else
                                let c_factor = factor ht (baseprefix, Gwdb.get_iper c) in
                                nodes := create_node ifam c gen Children baseprefix c_factor :: !nodes;
                                edges := create_edge p_factor base_prefix p c_factor baseprefix c :: !edges;
                                edges := create_edge sp_factor baseprefix sp c_factor baseprefix c :: !edges;
                                (baseprefix, c, gen + 1) :: acc
                            end acc children
                          end l (limit_list @@ !Geneweb.GWPARAM_ITL.get_children' conf base (Gwdb.get_iper p) fam (Gwdb.get_iper sp))
                        in
                        loop_child l
                  in
                  loop_child [(conf.Geneweb.Config.command, p, gen)]
                in
                child_local
              end else acc)
            l ifam
        in

        (* lien inter arbre *)
        let () = !Geneweb.GWPARAM_ITL.init_cache conf base (Gwdb.get_iper p) 1 1 (max_gen - gen) in
        let () =
          let ht = Hashtbl.create 0 in
          let rec loop_desc = function
            | [] -> ()
            | (base_prefix, p, gen) :: l ->
              if gen >= max_gen then loop_desc l
              else
                let p_factor = Option.value (Hashtbl.find_opt ht (base_prefix, Gwdb.get_iper p)) ~default:1 in
                let l =
                  List.fold_left begin fun acc (ifam, fam, (_ifath, _imoth, sp), baseprefix, can_merge) ->
                    if can_merge then acc
                    else
                      let sp_factor = factor ht (baseprefix, Gwdb.get_iper sp) in
                      nodes := create_node ifam sp gen Spouse baseprefix sp_factor :: !nodes;
                      edges := create_edge p_factor base_prefix p sp_factor baseprefix sp :: !edges;
                      List.fold_left begin fun acc (_baseprefix, _cpl, children) ->
                        List.fold_left begin fun acc ((c, _), _, _) ->
                          let c_factor = factor ht (baseprefix, Gwdb.get_iper c) in
                          nodes := create_node ifam c gen Children baseprefix c_factor :: !nodes;
                          edges := create_edge p_factor base_prefix p c_factor baseprefix c :: !edges;
                          edges := create_edge sp_factor baseprefix sp c_factor baseprefix c :: !edges;
                          (baseprefix, c, gen + 1) :: acc
                        end acc children
                      end acc (limit_list @@ !Geneweb.GWPARAM_ITL.get_children' conf base (Gwdb.get_iper p) fam (Gwdb.get_iper sp))
                  end l (!Geneweb.GWPARAM_ITL.get_families conf base p)
                in loop_desc l
          in
          loop_desc [(conf.Geneweb.Config.command, p, gen)]
        in

        loop l
  in
  nodes := create_node Gwdb.dummy_ifam p 1 Root conf.Geneweb.Config.command 1 :: !nodes;
  loop [(p, 1)];
  (* On retourne la liste pour avoir les noeuds dans l'ordre *)
  (* la référence, suivi du père suivi, puis de la mère ...  *)
  (List.rev !nodes, List.rev !edges)


let print_result_graph_tree
      (conf : Geneweb.Config.config) (base : Gwdb.base) (ip : Gwdb.iper)
    : unit =
  if Gwdb.iper_exists base ip then
  let params = Api_util.get_params conf Api_saisie_read_piqi_ext.parse_graph_tree_params in
  (* Construction de la base avec calcul des sosas           *)
  (* Si iz présent, on prend iz comme souche pour le calcul  *)
  (* Sinon on prend la souche de l'arbre                     *)
  let conf =
    match params.Api_saisie_read_piqi.Graph_tree_params.indexz with
    | Some n -> Api_util.set_sosa_ref conf (Gwdb.iper_of_string @@ Int32.to_string n)
    | None -> conf
  in
  let p = Gwdb.poi base ip in
  let max_asc = 12 in
  let nb_asc =
    match params.Api_saisie_read_piqi.Graph_tree_params.nb_asc with
    | Some n -> min max_asc (max (Int32.to_int n) 1)
    | None -> max_asc
  in
  (* cache lien inter arbre *)
  let () = !Geneweb.GWPARAM_ITL.init_cache conf base ip 1 1 1 in
  let (nodes_asc, edges_asc) = build_graph_asc conf base p nb_asc in
  let max_desc = 12 in
  let nb_desc =
    match params.Api_saisie_read_piqi.Graph_tree_params.nb_desc with
    | Some n -> min max_desc (max (Int32.to_int n) 1)
    | None -> max_desc
  in
  let (nodes_desc, edges_desc) = build_graph_desc conf base p nb_desc in
  let nodes_siblings =
    match Gwdb.get_parents p with
    | Some ifam ->
        let fam = Gwdb.foi base ifam in
        Array.fold_right
          (fun ic acc ->
            if ic = ip then acc
            else
              let c = Gwdb.poi base ic in
              (* Pour les liens inter arbres, on rend l'id unique avec *)
              (* le prefix de la base et l'index de la personne.       *)
              let uniq_id = Hashtbl.hash (conf.Geneweb.Config.command, ic) in
              let id = Int64.of_string @@ string_of_int uniq_id in
              let c = pers_to_piqi_person_tree conf base c Siblings 1 1 conf.Geneweb.Config.command in
              let node =
                { Api_saisie_read_piqi.Node.id = id
                ; person = c
                ; ifam = None
                }
              in
              node :: acc)
          (limit_array @@ Gwdb.get_children fam) []
    | None -> []
  in
  let (nodes_siblings_before, nodes_siblings_after) =
    match Gwdb.get_parents p with
    | Some ifam ->
        let fam = Gwdb.foi base ifam in
        let children = Array.to_list (limit_array @@ Gwdb.get_children fam) in
        let rec split_at_person before after l =
          match l with
          | [] -> (List.rev before, after)
          | ic :: l ->
              if ic = ip then
                let after =
                  List.map
                    (fun ic ->
                      let c = Gwdb.poi base ic in
                      (* Pour les liens inter arbres, on rend l'id unique avec *)
                      (* le prefix de la base et l'index de la personne.       *)
                      let uniq_id = Hashtbl.hash (conf.Geneweb.Config.command, ic) in
                      let id = Int64.of_string @@ string_of_int uniq_id in
                      let c = pers_to_piqi_person_tree conf base c Siblings 1 1 conf.Geneweb.Config.command in
                      { Api_saisie_read_piqi.Node.id = id
                      ; person = c
                      ; ifam = None
                      })
                    l
                in
                (List.rev before, after)
              else
                let c = Gwdb.poi base ic in
                (* Pour les liens inter arbres, on rend l'id unique avec *)
                (* le prefix de la base et l'index de la personne.       *)
                let uniq_id = Hashtbl.hash (conf.Geneweb.Config.command, ic) in
                let id = Int64.of_string @@ string_of_int uniq_id in
                let c = pers_to_piqi_person_tree conf base c Siblings 1 1 conf.Geneweb.Config.command in
                let node =
                  { Api_saisie_read_piqi.Node.id = id
                  ; person = c
                  ; ifam = None
                  }
                in
                split_at_person (node :: before) after l
        in
        split_at_person [] [] children
    | None -> ([], [])
  in
  let graph =
    Api_saisie_read_piqi.Graph_tree.({
      nodes_asc = nodes_asc;
      edges_asc = edges_asc;
      nodes_desc = nodes_desc;
      edges_desc = edges_desc;
      nodes_siblings = nodes_siblings;
      nodes_siblings_before = nodes_siblings_before;
      nodes_siblings_after = nodes_siblings_after;
    })
  in
  let data = Api_saisie_read_piqi_ext.gen_graph_tree graph in
  Api_util.print_result conf data
  else begin
    Geneweb.Output.status conf Def.Not_Found ;
    Geneweb.Output.print_sstring conf ""
  end

let get_nb_ancestors (base : Gwdb.base) (ip : Gwdb.iper) : int =
  (* Tableau qui conserve les index des personnes déjà parcourues. *)
  let visited_ips = Gwdb.iper_marker (Gwdb.ipers base) false in
  let rec count_nb_ancestors base not_visited_ips nb_visited_ips =
    match not_visited_ips with
      [] -> nb_visited_ips
      | current_ip::not_visited_ips ->
        if Gwdb.Marker.get visited_ips current_ip then
          (* Passe au noeud suivant si le noeud courant a déjà été visité. *)
          count_nb_ancestors base not_visited_ips nb_visited_ips
        else
          begin
            let not_visited_ips =
              match Gwdb.get_parents (Gwdb.poi base current_ip) with
              | Some ifam ->
                let cpl = Gwdb.foi base ifam in
                (* Ajoute les index des parents au tableau des noeuds à parcourir. *)
                not_visited_ips@[Gwdb.get_father cpl]@[Gwdb.get_mother cpl]
              | None ->
                (* Si pas de parents, le tableau des noeuds à visiter ne change pas. *)
                not_visited_ips
            in
            (* Met à jour le tableau des noeuds parcourus. *)
            Gwdb.Marker.set visited_ips current_ip true;
            (* Passe au noeud suivant en incrémentant le nombre de noeuds. *)
            count_nb_ancestors base not_visited_ips (nb_visited_ips + 1)
          end
  in
  (* Le nombre d'ascendants d'un individu est le nombre de personnes parcourues moins 1 (lui-même). *)
  count_nb_ancestors base [ip] (-1)

let nb_to_piqi_nb_ancestors (nb : int) : Piqirun_ext.output_format -> string =
    let piqi_nb_ancestors = Api_saisie_read_piqi.default_nb_ancestors() in
        piqi_nb_ancestors.Api_saisie_read_piqi.Nb_ancestors.nb <- Int32.of_int nb;
    Api_saisie_read_piqi_ext.gen_nb_ancestors piqi_nb_ancestors

let print_result_nb_ancestors
      (conf : Geneweb.Config.config) (base : Gwdb.base) (ip : Gwdb.iper)
    : unit =
    let data = nb_to_piqi_nb_ancestors (get_nb_ancestors base ip) in
    Api_util.print_result conf data

let print_nb_ancestors conf base =
  print_from_identifier_person conf base print_result_nb_ancestors (Api_util.get_params conf Api_saisie_read_piqi_ext.parse_identifier_person)

let print_graph_tree conf base =
  let params = Api_util.get_params conf Api_saisie_read_piqi_ext.parse_graph_tree_params in
  let identifier_person = params.Api_saisie_read_piqi.Graph_tree_params.identifier_person in
  print_from_identifier_person conf base print_result_graph_tree identifier_person

let get_paginated_data ~conf ~base params =
  let page =
    Api_util.Page.Piqi.from_page
      params.Api_saisie_read_piqi.Paginated_data_parameters.page
  in
  let person =
    params.Api_saisie_read_piqi.Paginated_data_parameters.person_id
    |> Int64.to_string
    |> Gwdb.iper_of_string
    |> Gwdb.poi base
  in
  let person_is_visible = Geneweb.Person.is_visible conf base person in
  match params.Api_saisie_read_piqi.Paginated_data_parameters.type_ with
  | `personal_event ->
     let events =
       fill_events
         ~page
         conf
         base
         person
         conf.Geneweb.Config.command
         person_is_visible
         pers_to_piqi_simple_person
         simple_witness_constructor
         get_event_constructor
     in
     `Personal_events (Api_util.Paginated_data.Piqi.to_personal_events events)
  | `witnessed_event ->
     let events =
       get_events_witnesses
         ~page
         conf
         base
         person
         conf.Geneweb.Config.command
         person_is_visible
         pers_to_piqi_simple_person
         simple_event_witness_constructor
     in
     `Witnessed_events (Api_util.Paginated_data.Piqi.to_witnessed_events events)
