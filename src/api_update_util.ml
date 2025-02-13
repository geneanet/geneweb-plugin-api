(**/**) (* Misc *)

type person_update = {
    first_name : string;
    surname : string;
    occurrence_number : int;
    kind : Geneweb.Update.create;
    force : bool;
  }

let get_local_occurrence_numbers, reserve_occurrence_number =
  let ht_free_occ = Hashtbl.create 33 in
  let key ~first_name ~surname = Name.lower (first_name ^ " " ^ surname) in
  let get_occurrence_numbers ~first_name ~surname =
    Option.value
      ~default:Ext_int.Set.empty
      (Hashtbl.find_opt ht_free_occ (key ~first_name ~surname))
  in
  let reserve_occurrence_number ~first_name ~surname occurrence_number =
    Hashtbl.replace
      ht_free_occ
      (key ~first_name ~surname)
      (Ext_int.Set.add
         occurrence_number
         (get_occurrence_numbers ~first_name ~surname))
  in
  (get_occurrence_numbers, reserve_occurrence_number)

let find_free_occ ~base ~first_name ~surname =
  let occ =
    let local_occurrence_numbers =
      get_local_occurrence_numbers ~first_name ~surname
    in
    let base_occurrence_numbers =
      Gutil.get_all_occurrence_numbers ~base ~first_name ~surname
    in
    Occurrence_number.smallest_free
      (Ext_int.Set.union local_occurrence_numbers base_occurrence_numbers)
  in
  reserve_occurrence_number ~first_name ~surname occ;
  occ


(**/**) (* Type de retour de modification. *)


(* Voir également update.mli, erreurs possibles :
     - "UnknownPerson"
     - "AlreadyDefined"
     - "OwnAncestor"
     - "BadSexOfMarriedPerson"
     - "BaseChanged"
     - "BadDateFormat"
     - "CreateConflictOcc"
     - "AlreadyHasParent"
     - "FatherShouldBeMale"
     - "MotherShouldBeFemale"
     - "Disconnected"
     - "error"
   On ajoute également les erreurs suivantes :
     - "PersonKey"
   => On ne renvoie en fait qu'une seule string qui est
      directement traduite du côté GeneWeb.
*)

type created_person = {
  n : string;
  p : string;
  oc : int32
}

let created_person_of_person base pers =
  let n = Gwdb.sou base pers.Def.surname in
  let p = Gwdb.sou base pers.Def.first_name in
  let oc = Int32.of_int pers.Def.occ in
  {n; p; oc}

let created_person ~n ~p ~oc = {n; p; oc}

let created_person_is_unnamed cp =
  cp.n = "?" && cp.p = "?"

type update_base_status =
  | UpdateSuccess of Geneweb.Warning.base_warning list * Geneweb.Warning.base_misc list * (unit -> unit) list * created_person option
  | UpdateError of Geneweb.Update.update_error
  | UpdateErrorConflict of Api_saisie_write_piqi.Create_conflict.t


(* Exception qui gère les conflits de création de personnes. *)
exception ModErrApiConflict of Api_saisie_write_piqi.Create_conflict.t

let error_conflict_person_link
      base
      created
      {first_name = f;
       surname = s;
       occurrence_number = o;
       kind = create;
       force = force_create} =
  let k = (f, s, o) in
  let exists () =
    Gwdb.person_of_key base f s o <> None || List.exists (Mutil.eq_key k) created
  in
  let f = if f = "" then "?" else f in
  let s = if s = "" then "?" else s in
  match create with
  | Geneweb.Update.Create (_, _) ->
    if f <> "?" && s <> "?" then
      if force_create then
        (* If we want to force, we need a free occ *)
        if exists ()
        then failwith "error_conflict_person_link"
        else false, k :: created
      else if exists () then true, created
      else false, k :: created
    else false, k :: created
  | Link -> false, created

let check_person_conflict base original_pevents sp =
  let op = Gwdb.poi base (sp.Def.key_index) in
  let ofn = Gwdb.sou base (Gwdb.get_first_name op) in
  let osn = Gwdb.sou base (Gwdb.get_surname op) in
  let oocc = Gwdb.get_occ op in
  let created =
    if ofn <> sp.Def.first_name || osn <> sp.Def.surname || oocc <> sp.Def.occ then
      match Gwdb.person_of_key base sp.Def.first_name sp.Def.surname sp.Def.occ with
      | Some p' when p' <> sp.Def.key_index ->
        let conflict =
          { Api_saisie_write_piqi.Create_conflict.form = Some `person_form1
          ; witness = false
          ; rparents = false
          ; event = false
          ; pos = None
          ; pos_witness = None
          ; lastname = sp.Def.surname
          ; firstname = sp.Def.first_name
          }
        in
        raise (ModErrApiConflict conflict)
      | _ -> [ (sp.Def.first_name, sp.Def.surname, sp.Def.occ) ]
    else []
  in
  (* Vérification des rparents. *)
  let _, created =
    List.fold_left begin fun (i, created) r ->
      match (r.Def.r_fath, r.Def.r_moth) with
      | (Some person_update, None)
      | (None, Some person_update) ->
        begin match error_conflict_person_link base created person_update with
          | true, _ ->
            let conflict =
              { Api_saisie_write_piqi.Create_conflict.form = Some `person_form1
              ; witness = false
              ; rparents = true
              ; event = false
              ; pos = Some (Int32.of_int i)
              ; pos_witness = None
              ; lastname = person_update.surname
              ; firstname = person_update.first_name
              }
            in
            raise (ModErrApiConflict conflict)
          | false, created ->
            (i + 1, created)
        end
      | None, None | Some _, Some _ ->
         failwith __LOC__ (* Dans l'API, ne peut pas arriver *)
    end (0, created) sp.rparents
  in
  (* Vérification des pevents. *)
  ignore @@
  List.fold_left begin fun created evt ->
    let _, created =
      Array.fold_left begin fun (j, created) (person_update, _, _) ->
        match error_conflict_person_link base created person_update with
        | true, _ ->
          let pos = Ext_list.index evt original_pevents in
          let conflict =
            { Api_saisie_write_piqi.Create_conflict.form = Some `person_form1
            ; witness = true
            ; rparents = false
            ; event = true
            ; pos = Some (Int32.of_int pos)
            ; pos_witness = Some (Int32.of_int j)
            ; lastname = person_update.surname
            ; firstname = person_update.first_name
            }
          in
          raise (ModErrApiConflict conflict)
        | false, created -> (j + 1, created)
      end (0, created) evt.Def.epers_witnesses
    in created
  end created sp.pevents

let check_family_conflict base sfam scpl sdes =
  let created = [] in
  let _, created =
    (* Vérification des parents. *)
    Array.fold_left begin fun (i, created) person_update ->
      match error_conflict_person_link base created person_update with
      | true, _ ->
        let conflict =
          { Api_saisie_write_piqi.Create_conflict.form = if i = 0 then Some `person_form1 else Some `person_form2
          ; witness = false
          ; rparents = false
          ; event = false
          ; pos = None
          ; pos_witness = None
          ; lastname = person_update.surname
          ; firstname = person_update.first_name
          }
        in
        raise (ModErrApiConflict conflict)
      | false, created -> (i + 1, created)
    end (0, created) (Adef.parent_array scpl)
  in
  let _, created =
    (* Vérification des fevents. *)
    List.fold_left begin fun (i, created) evt ->
      let _, created =
        Array.fold_left begin fun (j, created) (person_update, _wkind, _wnote) ->
          match error_conflict_person_link base created person_update with
          | true, _ ->
            let conflict =
              { Api_saisie_write_piqi.Create_conflict.form = Some `family_form
              ; witness = true
              ; rparents = false
              ; event = true
              ; pos = Some (Int32.of_int i)
              ; pos_witness = Some (Int32.of_int j)
              ; lastname = person_update.surname
              ; firstname = person_update.first_name
              }
            in
            raise (ModErrApiConflict conflict)
          | false, created -> (j + 1, created)
        end (0, created) evt.Def.efam_witnesses
      in
      (i + 1, created)
    end (0, created) sfam.Def.fevents
  in
  (* Vérification des enfants. *)
  ignore @@
  Array.fold_left begin fun created person_update ->
    match error_conflict_person_link base created person_update with
    | true, _ ->
      let conflict =
        { Api_saisie_write_piqi.Create_conflict.form = Some `person_form1
        ; witness = false
        ; rparents = false
        ; event = false
        ; pos = None
        ; pos_witness = None
        ; lastname = person_update.surname
        ; firstname = person_update.first_name
        }
      in
      raise (ModErrApiConflict conflict)
    | false, created -> created
  end created sdes.Def.children

(**/**) (* Convertion d'une date. *)


let piqi_date_of_date (date : Date.date) : Api_saisie_write_piqi.date =
  match date with
  | Date.Dgreg (dmy, cal) ->
      let (cal, dmy) =
        let d = Date.convert ~from:Date.Dgregorian ~to_:cal dmy in
        match cal with
        | Date.Dgregorian -> (None, dmy)
        | Date.Djulian -> (Some `julian, d)
        | Date.Dfrench -> (Some `french, d)
        | Date.Dhebrew -> (Some `hebrew, d)
      in
      let (prec, dmy, dmy2) =
        let d = Some (Int32.of_int dmy.day) in
        let m = Some (Int32.of_int dmy.month) in
        let y = Some (Int32.of_int dmy.year) in
        let delta = Some (Int32.of_int dmy.delta) in
        let dmy1 = {Api_saisie_write_piqi.Dmy.day = d; month = m; year = y; delta = delta;} in
        let (prec, dmy2) =
          match dmy.prec with
          | Date.Sure -> (`sure, None)
          | Date.About -> (`about, None)
          | Date.Maybe -> (`maybe, None)
          | Date.Before -> (`before, None)
          | Date.After -> (`after, None)
          | Date.OrYear dmy2 ->
              let d = Some (Int32.of_int dmy2.day2) in
              let m = Some (Int32.of_int dmy2.month2) in
              let y = Some (Int32.of_int dmy2.year2) in
              let delta = Some (Int32.of_int dmy2.delta2) in
              let dmy2 =
                {Api_saisie_write_piqi.Dmy.day = d; month = m; year = y; delta = delta;}
              in
              (`oryear, Some dmy2)
          | Date.YearInt dmy2 ->
              let d = Some (Int32.of_int dmy2.day2) in
              let m = Some (Int32.of_int dmy2.month2) in
              let y = Some (Int32.of_int dmy2.year2) in
              let delta = Some (Int32.of_int dmy2.delta2) in
              let dmy2 =
                {Api_saisie_write_piqi.Dmy.day = d; month = m; year = y; delta = delta;}
              in
              (`yearint, Some dmy2)
        in
        (prec, dmy1, dmy2)
      in
      {
        Api_saisie_write_piqi.Date.cal = cal;
        prec = Some prec;
        dmy = Some dmy;
        dmy2 = dmy2;
        text = None;
      }
  | Date.Dtext txt ->
      {
        Api_saisie_write_piqi.Date.cal = None;
        prec = None;
        dmy = None;
        dmy2 = None;
        text = Some (Utf8.normalize txt);
      }


let date_of_piqi_date conf date =
  match date.Api_saisie_write_piqi.Date.text with
  | Some txt -> Some (Date.Dtext txt)
  | None ->
      (* Si on a une année, on a une date. *)
      match date.Api_saisie_write_piqi.Date.dmy with
      | Some dmy ->
          begin
            match dmy.Api_saisie_write_piqi.Dmy.year with
            | Some _ ->
                let cal =
                  Option.fold
                    date.Api_saisie_write_piqi.Date.cal
                    ~some:Api_util.calendar_of_piqi_calendar
                    ~none:Date.Dgregorian
                in
                let get_adef_dmy_from_saisie_write_dmy_if_valid conf dmy cal prec =
                  let day =
                    match dmy.Api_saisie_write_piqi.Dmy.day with
                    | Some day -> Int32.to_int day
                    | None -> 0
                  in
                  let month =
                    match dmy.Api_saisie_write_piqi.Dmy.month with
                    | Some month -> Int32.to_int month
                    | None -> 0
                  in
                  let year =
                    match dmy.Api_saisie_write_piqi.Dmy.year with
                    | Some year -> Int32.to_int year
                    | None -> 0
                  in
                  let delta =
                    match dmy.Api_saisie_write_piqi.Dmy.delta with
                    | Some delta -> Int32.to_int delta
                    | None -> 0
                  in
                  (* Error handling. *)
                  let (day, month, year) =
                    if year = 0 && month <= 0
                    then
                        (0, 0, year)
                    else
                        (day, month, year)
                  in
                  let adef_dmy =
                    {Date.day = day; month = month; year = year; delta = delta; prec = prec}
                  in
                  let day_to_check =
                    adef_dmy.day >= 1 && adef_dmy.day <= 31
                  in
                  let month_to_check =
                    adef_dmy.month >= 1 && adef_dmy.month <= 13
                  in
                  (* Returns date directy if there is no month. *)
                  if adef_dmy.month = 0 then adef_dmy
                  (* If no specified day, checks the month value. *)
                  else if adef_dmy.day = 0 && month_to_check
                  then
                    (* Check the month in the gregorian calendar. *)
                    if cal = Date.Dgregorian
                    then
                      begin
                        (* The day is set to 1 for checking. *)
                        Geneweb.Update.check_greg_day conf {Date.day = 1; month = adef_dmy.month; year = adef_dmy.year; delta = delta; prec = prec};
                        adef_dmy
                      end
                    else
                      adef_dmy
                  (* Day and month are specified here. *)
                  else if day_to_check && month_to_check
                  then
                    (* Check the date in the gregorian calendar. *)
                    if cal = Date.Dgregorian
                    then
                      begin
                        Geneweb.Update.check_greg_day conf {Date.day = adef_dmy.day; month = adef_dmy.month; year = adef_dmy.year; delta = delta; prec = prec};
                        adef_dmy
                      end
                    else
                      adef_dmy
                  else
                    Geneweb.Update.bad_date conf (`Date adef_dmy)
                in
                let delta2 = 0 in
                let prec =
                  match date.Api_saisie_write_piqi.Date.prec with
                  | Some `about -> Date.About
                  | Some `maybe -> Date.Maybe
                  | Some `before -> Date.Before
                  | Some `after -> Date.After
                  | Some `oryear ->
                      (match date.Api_saisie_write_piqi.Date.dmy2 with
                      | Some dmy ->
                          begin
                            match dmy.Api_saisie_write_piqi.Dmy.year with
                            | Some _ ->
                              let adef_dmy = get_adef_dmy_from_saisie_write_dmy_if_valid conf dmy cal Date.Sure in
                              Date.OrYear {day2 = adef_dmy.day; month2 = adef_dmy.month; year2 = adef_dmy.year; delta2 = delta2}
                            | None -> Date.Sure
                          end
                      | None -> Date.Sure (* erreur*))
                  | Some `yearint ->
                      (match date.Api_saisie_write_piqi.Date.dmy2 with
                      | Some dmy ->
                          begin
                            match dmy.Api_saisie_write_piqi.Dmy.year with
                            | Some _ ->
                              let adef_dmy = get_adef_dmy_from_saisie_write_dmy_if_valid conf dmy cal Date.Sure in
                              Date.YearInt {day2 = adef_dmy.day; month2 = adef_dmy.month; year2 = adef_dmy.year; delta2 = delta2}
                            | None -> Date.Sure
                          end
                      | None -> Date.Sure (* erreur*))
                  | Some `sure | None -> Date.Sure
                in
                let dmy =
                  match date.Api_saisie_write_piqi.Date.dmy with
                  | Some dmy ->
                      get_adef_dmy_from_saisie_write_dmy_if_valid conf dmy cal prec
                  | None -> (* erreur*)
                      {Date.day = 0; month = 0; year = 0; prec = Sure; delta = 0}
                in
                let dmy =
                  begin match cal with
                  | Date.Dgregorian ->
                      Geneweb.Update.check_greg_day conf dmy
                  | Date.Djulian | Date.Dfrench | Date.Dhebrew -> ()
                  end;
                  Date.convert ~from:cal ~to_:Date.Dgregorian dmy
                in
                Some (Date.Dgreg (dmy, cal))
          | None -> None
          end
      | None -> None


(**/**) (* Convertion d'une personne pour la lecture. *)


(* Copie de util.ml pour supprimer le html *)

let child_of_parent conf base p =
  (* Si le père a un nom de famille différent de la personne *)
  (* alors on l'affiche, sinon on n'affiche que le prénom.   *)
  let print_father fath =
    if not (Gwdb.eq_istr (Gwdb.get_surname p) (Gwdb.get_surname fath)) then
      Geneweb.NameDisplay.fullname_str_of_person conf base fath
    else
      Geneweb.NameDisplay.first_name_str_of_person conf base fath
  in
  let a = Geneweb.Util.pget conf base (Gwdb.get_iper p) in
  let ifam =
    match Gwdb.get_parents a with
    | Some ifam ->
        let cpl = Gwdb.foi base ifam in
        let fath =
          let fath = Geneweb.Util.pget conf base (Gwdb.get_father cpl) in
          if Gwdb.p_first_name base fath = "?" then None else Some fath
        in
        let moth =
          let moth = Geneweb.Util.pget conf base (Gwdb.get_mother cpl) in
          if Gwdb.p_first_name base moth = "?" then None else Some moth
        in
        Some (fath, moth)
    | None -> None
  in
  match ifam with
  | Some (None, None) | None -> ""
  | Some (fath, moth) ->
      let s =
        match (fath, moth) with
        | (Some fath, None) -> print_father fath
        | (None, Some moth) -> Geneweb.NameDisplay.fullname_str_of_person conf base moth
        | (Some fath, Some moth) ->
          print_father fath
          ^ " " ^ Geneweb.Util.transl_nth conf "and" 0 ^ " "
          ^ Geneweb.NameDisplay.fullname_str_of_person conf base moth
        | _ -> ""
      in
      let is = Geneweb.Util.index_of_sex (Gwdb.get_sex p) in
      Geneweb.Util.translate_eval
        (Geneweb.Util.transl_a_of_gr_eq_gen_lev conf
           (Geneweb.Util.transl_nth conf "son/daughter/child" is)
           (s :> string)
           (s :> string))

let husband_wife conf base p =
  let rec loop i =
    if i < Array.length (Gwdb.get_family p) then
      let fam = Gwdb.foi base (Gwdb.get_family p).(i) in
      let conjoint = Gutil.spouse (Gwdb.get_iper p) fam in
      let conjoint = Geneweb.Util.pget conf base conjoint in
      if Gwdb.p_first_name base conjoint <> "?" || Gwdb.p_surname base conjoint <> "?"
      then
        let relation =
          Printf.sprintf (Geneweb.Util.relation_txt conf (Gwdb.get_sex p) fam) (fun () -> "")
        in
        Geneweb.Util.translate_eval
          (relation ^ " " ^ (Geneweb.NameDisplay.fullname_str_of_person conf base conjoint))
      else loop (i + 1)
    else ""
  in
  loop 0


let pers_to_piqi_simple_person (conf : Geneweb.Config.config) (base : Gwdb.base) (p : Gwdb.person) : Api_saisie_write_piqi.simple_person =
  let index = Int32.of_string @@ Gwdb.string_of_iper (Gwdb.get_iper p) in
  let sex =
    match Gwdb.get_sex p with
    | Def.Male -> `male
    | Def.Female -> `female
    | Def.Neuter -> `unknown
  in
  let sosa =
    let sosa_nb = Geneweb.Sosa_cache.get_sosa_person ~conf ~base ~person:p in
    if Sosa.eq sosa_nb Sosa.zero then `no_sosa
    else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
    else `sosa
  in
  let (first_name, surname) =
    Api_saisie_read.person_firstname_surname_txt base p
  in
  let (birth_short, birth_place, death_short, death_place) =
    let (birth, death, _) = Gutil.get_birth_death_date p in
    let birth =
      match birth with
      | Some d ->
         let open Api_util in
         !!(Geneweb.DateDisplay.string_slash_of_date conf d)
      | None -> ""
    in
    let birth_place =
      let birth_place = Gwdb.sou base (Gwdb.get_birth_place p) in
      if birth_place <> ""
      then
        let open Api_util in
        !!(Geneweb.Util.string_of_place birth_place)
      else
        let baptism_place = Gwdb.sou base (Gwdb.get_baptism_place p) in
        let open Api_util in
        !!(Geneweb.Util.string_of_place baptism_place)
    in
    let death =
      match death with
      | Some d ->
         let open Api_util in
         !!(Geneweb.DateDisplay.string_slash_of_date conf d)
      | None -> ""
    in
    let death_place =
      let death_place = Gwdb.sou base (Gwdb.get_death_place p) in
      if death_place <> ""
      then
        let open Api_util in
        !!(Geneweb.Util.string_of_place death_place)
      else
        let burial_place = Gwdb.sou base (Gwdb.get_burial_place p) in
        let open Api_util in
        !!(Geneweb.Util.string_of_place burial_place)
    in
    (birth, birth_place, death, death_place)
  in
  let image = Api_util.get_portrait conf base p in
  {
    Api_saisie_write_piqi.Simple_person.index = index;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    birth_short_date = if birth_short = "" then None else Some birth_short;
    birth_place = if birth_place = "" then None else Some birth_place;
    death_short_date = if death_short = "" then None else Some death_short;
    death_place = if death_place = "" then None else Some death_place;
    image;
    sosa = sosa;
  }


let pers_to_piqi_person_search conf base p =
  let index = Int32.of_string @@ Gwdb.string_of_iper (Gwdb.get_iper p) in
  let sex =
    match Gwdb.get_sex p with
    | Def.Male -> `male
    | Def.Female -> `female
    | Def.Neuter -> `unknown
  in
  let sosa =
    let sosa_nb = Geneweb.Sosa_cache.get_sosa_person ~conf ~base ~person:p in
    if Sosa.eq sosa_nb Sosa.zero then `no_sosa
    else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
    else `sosa
  in
  let (first_name, surname) =
    Api_saisie_read.person_firstname_surname_txt base p
  in
  let dates = Api_saisie_read.short_dates_text conf base p in
  let image = Api_util.get_portrait conf base p in
  let family =
    let hw = husband_wife conf base p in
    if hw <> "" then hw
    else child_of_parent conf base p
  in
  {
    Api_saisie_write_piqi.Person_search.index = index;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    dates = if dates = "" then None else Some dates;
    image;
    sosa = sosa;
    family = family;
  }


let pers_to_piqi_person_search_info conf base p =
  let index = Int32.of_string @@ Gwdb.string_of_iper (Gwdb.get_iper p) in
  let sex =
    match Gwdb.get_sex p with
    | Def.Male -> `male
    | Def.Female -> `female
    | Def.Neuter -> `unknown
  in
  let sosa =
    let sosa_nb = Geneweb.Sosa_cache.get_sosa_person ~conf ~base ~person:p in
    if Sosa.eq sosa_nb Sosa.zero then `no_sosa
    else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
    else `sosa
  in
  let surname = Gwdb.sou base (Gwdb.get_surname p) in
  let first_name = Gwdb.sou base (Gwdb.get_first_name p) in
  let publicname = Gwdb.sou base (Gwdb.get_public_name p) in
  let aliases = List.map (Gwdb.sou base) (Gwdb.get_aliases p) in
  let qualifiers = List.map (Gwdb.sou base) (Gwdb.get_qualifiers p) in
  let firstname_aliases = List.map (Gwdb.sou base) (Gwdb.get_first_names_aliases p) in
  let surname_aliases = List.map (Gwdb.sou base) (Gwdb.get_surnames_aliases p) in
  let occupation =
    let open Api_util in
    !!(Geneweb.Notes.source conf base (Gwdb.sou base (Gwdb.get_occupation p)))
  in
  let image = Api_util.get_portrait conf base p in
  let events =
    List.map
      (fun evt ->
        let name =
          match Geneweb.Event.get_name evt with
          | Geneweb.Event.Pevent name ->
             let open Api_util in
             !!(Geneweb.Util.string_of_pevent_name conf base name)
          | Geneweb.Event.Fevent name ->
             let open Api_util in
             !!(Geneweb.Util.string_of_fevent_name conf base name)
        in
        let (date, _, date_conv, _, date_cal) =
          match Date.od_of_cdate (Geneweb.Event.get_date evt) with
          | Some d -> Api_saisie_read.string_of_date_and_conv conf d
          | None -> ("", "", "", "", None)
        in
        let place =
          let open Api_util in
          !!(Geneweb.Util.raw_string_of_place (Gwdb.sou base (Geneweb.Event.get_place evt)) |> Adef.safe)
        in
        let note =
          let open Api_util in
          !!(Geneweb.Notes.person_note conf base p (Gwdb.sou base (Geneweb.Event.get_note evt)))
        in
        let src =
          let open Api_util in
          !!(Geneweb.Notes.source conf base (Gwdb.sou base (Geneweb.Event.get_src evt)))
        in
        let spouse =
          match Geneweb.Event.get_spouse_iper evt with
          | Some ip ->
              let sp = Gwdb.poi base ip in
              Some (pers_to_piqi_simple_person conf base sp)
          | None -> None
        in
        let witnesses =
          Mutil.array_to_list_map
            (fun (ip, wk, wnote) ->
              let witness_type = Api_util.piqi_of_witness_kind wk in
               let witness = pers_to_piqi_simple_person conf base @@ Gwdb.poi base ip in
              let witness_note = Gwdb.sou base wnote in
              let witness_note = if witness_note = "" then None else Some witness_note in
              Api_saisie_write_piqi.Witness_event.{ witness_type ; witness ; witness_note})
            (Geneweb.Event.get_witnesses_and_notes evt)
        in
        {
          Api_saisie_write_piqi.Event.name = name;
          date = if date = "" then None else Some date;
          date_conv = if date_conv = "" then None else Some date_conv;
          date_cal = date_cal;
          place = if place = "" then None else Some place;
          reason = None;
          note = if note = "" then None else Some note;
          src = if src= "" then None else Some src;
          spouse = spouse;
          witnesses = witnesses;
        })
      (Geneweb.Event.sorted_events conf base p)
  in
  let notes =
    let open Api_util in
    !!(Geneweb.Notes.person_note conf base p (Gwdb.sou base (Gwdb.get_notes p)))
  in
  let psources =
    let open Api_util in
    !!(Geneweb.Notes.source conf base (Gwdb.sou base (Gwdb.get_psources p)))
  in
  let has_sources = psources <> "" in
  let titles = Geneweb.Perso.nobility_titles_list conf base p in
  let titles =
    List.map (fun x ->
        let open Api_util in
        !!(Geneweb.Perso.string_of_title ~safe:true ~link:false conf base (Adef.safe "") p x)
      ) titles
  in
  let related =
    let list =
      let list = List.sort_uniq compare (Gwdb.get_related p) in
      List.fold_left
        (fun list ic ->
           let c = Geneweb.Util.pget conf base ic in
           let rec loop list =
             function
             | r :: rl ->
                 (match r.Def.r_fath with
                 | Some ip when ip = Gwdb.get_iper p ->
                     loop ((c, r) :: list) rl
                 | _ ->
                     (match r.Def.r_moth with
                     | Some ip when ip = Gwdb.get_iper p ->
                         loop ((c, r) :: list) rl
                     | _ -> loop list rl))
             | [] -> list
           in loop list (Gwdb.get_rparents c))
        [] list
    in
    let list =
      List.sort
        (fun (c1, _) (c2, _) ->
           let d1 =
             match Date.od_of_cdate (Gwdb.get_baptism c1) with
             | None -> Date.od_of_cdate (Gwdb.get_birth c1)
             | x -> x
           in
           let d2 =
             match Date.od_of_cdate (Gwdb.get_baptism c2) with
             | None -> Date.od_of_cdate (Gwdb.get_birth c2)
             | x -> x
           in
           match (d1, d2) with
           |(Some d1, Some d2) -> Date.compare_date d1 d2
           | Some _, None | None, (None | Some _) -> -1 )
      (List.rev list)
    in
    List.map
      (fun (p, rp) ->
        let p = pers_to_piqi_simple_person conf base p in
        let r_type =
          match rp.Def.r_type with
          | Adoption -> `rchild_adoption
          | Recognition -> `rchild_recognition
          | CandidateParent -> `rchild_candidate_parent
          | GodParent -> `rchild_god_parent
          | FosterParent -> `rchild_foster_parent
        in
        {
          Api_saisie_write_piqi.Relation_person.r_type = r_type;
          person = p;
        } )
      list
  in
  let rparents =
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
        let rl =
          match rp.Def.r_fath with
          | Some ip ->
              let p = Gwdb.poi base ip in
              let p = pers_to_piqi_simple_person conf base p in
              let p =
                {
                  Api_saisie_write_piqi.Relation_person.r_type = r_type;
                  person = p;
                }
              in
              p :: rl
          | None -> rl
        in
        match rp.Def.r_moth with
        | Some ip ->
          let p = Gwdb.poi base ip in
          let p = pers_to_piqi_simple_person conf base p in
          let p =
            {
              Api_saisie_write_piqi.Relation_person.r_type = r_type;
              person = p;
            }
          in
          p :: rl
        | None -> rl)
      [] (Gwdb.get_rparents p)
  in
  let was_witness =
    let list =
      let list = ref [] in
      let related = List.sort_uniq compare (Gwdb.get_related p) in
      let rec make_list =
        function
        | ic :: icl ->
            let c = Geneweb.Util.pget conf base ic in
            if Gwdb.get_sex c = Def.Male then
              Array.iter
                (fun ifam ->
                   let fam = Gwdb.foi base ifam in
                   if Array.mem (Gwdb.get_iper p) (Gwdb.get_witnesses fam)
                   then
                     list := (ifam, fam) :: !list
                   else ())
                (Gwdb.get_family (Geneweb.Util.pget conf base ic))
            else ();
            make_list icl
        | [] -> ()
      in
      make_list related;
      !list
    in
    let list =
      List.sort
        (fun (_, fam1) (_, fam2) ->
           match
             (Date.od_of_cdate (Gwdb.get_marriage fam1),
              Date.od_of_cdate (Gwdb.get_marriage fam2))
           with
           | (Some d1, Some d2) -> Date.compare_date d1 d2
           | Some _, None | None, (None | Some _) -> 0 )
        list
    in
    List.map
      (fun (_, fam) ->
         let ifath = Gwdb.get_father fam in
         let imoth = Gwdb.get_mother fam in
         let father = Gwdb.poi base ifath in
         let mother = Gwdb.poi base imoth in
         let father_auth = Geneweb.Util.authorized_age conf base father in
         let husband =
           if not father_auth && (Geneweb.Util.is_hide_names conf father) then "x x"
           else Gwdb.p_first_name base father ^ " " ^ Gwdb.p_surname base father
         in
         let mother_auth = Geneweb.Util.authorized_age conf base mother in
         let wife =
           if not mother_auth && (Geneweb.Util.is_hide_names conf mother) then "x x"
           else Gwdb.p_first_name base mother ^ " " ^ Gwdb.p_surname base mother
         in
         Api_saisie_write_piqi.Was_witness.({
           husband = husband;
           wife = wife;
         }) )
      list
  in
  {
    Api_saisie_write_piqi.Person_search_info.index = index;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    public_name = if publicname = "" then None else Some publicname;
    aliases = aliases;
    qualifiers = qualifiers;
    firstname_aliases = firstname_aliases;
    surname_aliases = surname_aliases;
    image;
    events = events;
    occupation = if occupation = "" then None else Some occupation;
    notes = if notes = "" then None else Some notes;
    psources = if psources = "" then None else Some psources;
    has_sources = has_sources;
    titles = titles;
    related = related;
    rparents = rparents;
    was_witness = was_witness;
    sosa = sosa;
  }


(**/**) (* Convertion d'une personne, d'une famille. *)


let pers_to_piqi_person_link conf base p =
  let create_link = `link in
  let index = Int32.of_string @@ Gwdb.string_of_iper (Gwdb.get_iper p) in
  let sex =
    match Gwdb.get_sex p with
    | Def.Male -> `male
    | Def.Female -> `female
    | Def.Neuter -> `unknown
  in
  let first_name = Gwdb.sou base (Gwdb.get_first_name p) in
  let surname = Gwdb.sou base (Gwdb.get_surname p) in
  let occ = Gwdb.get_occ p in
  let occ = if occ = 0 then None else Some (Int32.of_int occ) in
  let dates = Api_saisie_read.short_dates_text conf base p in
  let dates =
    if dates = "" then None
    else Some ("(" ^ dates ^ ")")
  in
  {
    Api_saisie_write_piqi.Person_link.create_link = create_link;
    index = index;
    sex = sex;
    lastname = Utf8.normalize surname;
    firstname = Utf8.normalize first_name;
    occ = occ;
    dates = Option.map Utf8.normalize dates;
  }


let pers_to_piqi_mod_person conf base p =
  let digest = Geneweb.Update.digest_person (Geneweb.UpdateInd.string_person_of base p) in
  let create_link = `link in
  let index = Int32.of_string @@ Gwdb.string_of_iper (Gwdb.get_iper p) in
  let sex =
    match Gwdb.get_sex p with
    | Def.Male -> `male
    | Def.Female -> `female
    | Def.Neuter -> `unknown
  in
  let surname = Gwdb.sou base (Gwdb.get_surname p) in
  let first_name = Gwdb.sou base (Gwdb.get_first_name p) in
  let occ = Gwdb.get_occ p in
  let occ =
    (* Cas particulier pour les personnes sans clé, et principalement *)
    (* ? ?. On ne renvoie pas le occ, comme ça si la personne existe  *)
    (* dans la base, on aura le droit à un conflit de nom.            *)
    if Name.lower surname = "" || Name.lower first_name = "" then None
    else
      if occ = 0 then None
      else Some (Int32.of_int occ)
  in
  let publicname = Gwdb.sou base (Gwdb.get_public_name p) in
  let aliases = List.map (Gwdb.sou base) (Gwdb.get_aliases p) in
  let qualifiers = List.map (Gwdb.sou base) (Gwdb.get_qualifiers p) in
  let firstname_aliases = List.map (Gwdb.sou base) (Gwdb.get_first_names_aliases p) in
  let surname_aliases = List.map (Gwdb.sou base) (Gwdb.get_surnames_aliases p) in
  let image = Api_util.get_portrait conf base p in
  let death_type = Api_util.piqi_death_type_of_death (Gwdb.get_death p) in
  let occupation = Gwdb.sou base (Gwdb.get_occupation p) in
  let psources = Gwdb.sou base (Gwdb.get_psources p) in
  let notes = Gwdb.sou base (Gwdb.get_notes p) in
  let titles =
    List.map
      (fun t ->
        (* On ne prend pas en compte le type du titre (main/name/none). *)
        let name =
          match t.Def.t_name with
          | Tmain -> ""
          | Tname name -> Utf8.normalize (Gwdb.sou base name)
          | Tnone -> ""
        in
        let title = Utf8.normalize (Gwdb.sou base t.t_ident) in
        let fief = Utf8.normalize (Gwdb.sou base t.t_place) in
        let date_begin =
          match Date.od_of_cdate t.t_date_start with
          | Some d -> Some (piqi_date_of_date d)
          | None -> None
        in
        let date_end =
          match Date.od_of_cdate t.t_date_end with
          | Some d -> Some (piqi_date_of_date d)
          | None -> None
        in
        let nth = Some (Int32.of_int t.t_nth) in
        Api_saisie_write_piqi.Title.({
          name = if name = "" then None else Some name;
          title = if title = "" then None else Some title;
          fief = if fief = "" then None else Some fief;
          date_begin = date_begin;
          date_end = date_end;
          nth = nth;
        }))
      (Gwdb.get_titles p)
  in
  let pevents =
    List.map
      (fun evt ->
         let (pevent_type, event_perso) =
           match Gwdb.get_pevent_name evt with
           | Epers_Birth -> (Some `epers_birth, None)
           | Epers_Baptism -> (Some `epers_baptism, None)
           | Epers_Death -> (Some `epers_death, None)
           | Epers_Burial -> (Some `epers_burial, None)
           | Epers_Cremation -> (Some `epers_cremation, None)
           | Epers_Accomplishment -> (Some `epers_accomplishment, None)
           | Epers_Acquisition -> (Some `epers_acquisition, None)
           | Epers_Adhesion -> (Some `epers_adhesion, None)
           | Epers_BaptismLDS -> (Some `epers_baptismlds, None)
           | Epers_BarMitzvah -> (Some `epers_barmitzvah, None)
           | Epers_BatMitzvah -> (Some `epers_batmitzvah, None)
           | Epers_Benediction -> (Some `epers_benediction, None)
           | Epers_ChangeName -> (Some `epers_changename, None)
           | Epers_Circumcision -> (Some `epers_circumcision, None)
           | Epers_Confirmation -> (Some `epers_confirmation, None)
           | Epers_ConfirmationLDS -> (Some `epers_confirmationlds, None)
           | Epers_Decoration -> (Some `epers_decoration, None)
           | Epers_DemobilisationMilitaire -> (Some `epers_demobilisationmilitaire, None)
           | Epers_Diploma -> (Some `epers_diploma, None)
           | Epers_Distinction -> (Some `epers_distinction, None)
           | Epers_Dotation -> (Some `epers_dotation, None)
           | Epers_DotationLDS -> (Some `epers_dotationlds, None)
           | Epers_Education -> (Some `epers_education, None)
           | Epers_Election -> (Some `epers_election, None)
           | Epers_Emigration -> (Some `epers_emigration, None)
           | Epers_Excommunication -> (Some `epers_excommunication, None)
           | Epers_FamilyLinkLDS -> (Some `epers_familylinklds, None)
           | Epers_FirstCommunion -> (Some `epers_firstcommunion, None)
           | Epers_Funeral -> (Some `epers_funeral, None)
           | Epers_Graduate -> (Some `epers_graduate, None)
           | Epers_Hospitalisation -> (Some `epers_hospitalisation, None)
           | Epers_Illness -> (Some `epers_illness, None)
           | Epers_Immigration -> (Some `epers_immigration, None)
           | Epers_ListePassenger -> (Some `epers_listepassenger, None)
           | Epers_MilitaryDistinction -> (Some `epers_militarydistinction, None)
           | Epers_MilitaryPromotion -> (Some `epers_militarypromotion, None)
           | Epers_MilitaryService -> (Some `epers_militaryservice, None)
           | Epers_MobilisationMilitaire -> (Some `epers_mobilisationmilitaire, None)
           | Epers_Naturalisation -> (Some `epers_naturalisation, None)
           | Epers_Occupation -> (Some `epers_occupation, None)
           | Epers_Ordination -> (Some `epers_ordination, None)
           | Epers_Property -> (Some `epers_property, None)
           | Epers_Recensement -> (Some `epers_recensement, None)
           | Epers_Residence-> (Some `epers_residence, None)
           | Epers_Retired -> (Some `epers_retired, None)
           | Epers_ScellentChildLDS -> (Some `epers_scellentchildlds, None)
           | Epers_ScellentParentLDS -> (Some `epers_scellentparentlds, None)
           | Epers_ScellentSpouseLDS -> (Some `epers_scellentspouselds, None)
           | Epers_VenteBien -> (Some `epers_ventebien, None)
           | Epers_Will -> (Some `epers_will, None)
           | Epers_Name n -> (None, Some (Utf8.normalize (Gwdb.sou base n)))
         in
         let date =
           match Date.od_of_cdate (Gwdb.get_pevent_date evt) with
           | Some d -> Some (piqi_date_of_date d)
           | None -> None
         in
         let place = Gwdb.sou base (Gwdb.get_pevent_place evt) in
         let reason = None in
         let note = Gwdb.sou base (Gwdb.get_pevent_note evt) in
         let src = Gwdb.sou base (Gwdb.get_pevent_src evt) in
         let witnesses =
           Mutil.array_to_list_map
             (fun (ip, wk, wnote) ->
                let witness_type = Api_util.piqi_of_witness_kind wk in
                let p = Gwdb.poi base ip in
                let person_link = pers_to_piqi_person_link conf base p in
                let witness_note = Utf8.normalize (Gwdb.sou base wnote) in
                let witness_note = if witness_note = "" then None else Some witness_note in
                Api_saisie_write_piqi.Witness.{ witness_type ; person = Some person_link; witness_note })
             (Gwdb.get_pevent_witnesses_and_notes evt)
         in
         {
           Api_saisie_write_piqi.Pevent.pevent_type = pevent_type;
           date = date;
           place = if place = "" then None else Some (Utf8.normalize place);
           reason = reason;
           note = if note = "" then None else Some (Utf8.normalize note);
           src = if src = "" then None else Some (Utf8.normalize src);
           witnesses = witnesses;
           event_perso = event_perso;
         })
      (Geneweb.Event.sort_events
         (fun e -> Geneweb.Event.Pevent (Gwdb.get_pevent_name e))
         (fun e -> Gwdb.get_pevent_date e)
         (Gwdb.get_pevents p)
      )
  in
  (* Si la personne n'a aucun évènement et/ou est décédée mais *)
  (* sans évènement, on ajoute les évènements nécessaires.     *)
  let pevents =
    if pevents = [] then
      begin
        let birth =
          {
            Api_saisie_write_piqi.Pevent.pevent_type = Some `epers_birth;
            date = None;
            place = None;
            reason = None;
            note = None;
            src = None;
            witnesses = [];
            event_perso = None;
          }
        in
        (* Que pour les personnes qui existent. *)
        if Gwdb.get_iper p <> Gwdb.dummy_iper && death_type != `not_dead then
          let death =
            {
              Api_saisie_write_piqi.Pevent.pevent_type = Some `epers_death;
              date = None;
              place = None;
              reason = None;
              note = None;
              src = None;
              witnesses = [];
              event_perso = None;
            }
          in
          [birth; death]
        else [birth]
      end
    else
      let (has_birth, has_death) =
        List.fold_left
          (fun (has_birth, has_death) evt ->
            (has_birth || Gwdb.get_pevent_name evt = Epers_Birth,
             has_death || Gwdb.get_pevent_name evt = Epers_Death))
          (false, false) (Gwdb.get_pevents p)
      in
      let pevents =
        if has_birth then pevents
        else
          begin
            let birth =
              {
                Api_saisie_write_piqi.Pevent.pevent_type = Some `epers_birth;
                date = None;
                place = None;
                reason = None;
                note = None;
                src = None;
                witnesses = [];
                event_perso = None;
              }
            in
            birth :: pevents
          end
      in
      if has_death || death_type = `not_dead  then pevents
      else
        begin
          let death =
            {
              Api_saisie_write_piqi.Pevent.pevent_type = Some `epers_death;
              date = None;
              place = None;
              reason = None;
              note = None;
              src = None;
              witnesses = [];
              event_perso = None;
            }
          in
          pevents @ [death]
        end;
  in
  let related = List.map (fun x -> Int32.of_string @@ Gwdb.string_of_iper x) (Gwdb.get_related p) in
  let rparents =
    List.fold_right
      (fun rp accu ->
        let source = Gwdb.sou base rp.Def.r_sources in
        let accu =
          match rp.Def.r_fath with
          | Some ip ->
              let p = Gwdb.poi base ip in
              let father = pers_to_piqi_person_link conf base p in
              let rpt_type =
                match rp.Def.r_type with
                | Adoption -> `rpt_adoption_father
                | Recognition -> `rpt_recognition_father
                | CandidateParent -> `rpt_candidate_parent_father
                | GodParent -> `rpt_god_parent_father
                | FosterParent -> `rpt_foster_parent_father
              in
              let r =
                Api_saisie_write_piqi.Relation_parent.({
                  rpt_type = rpt_type;
                  person = Some father;
                  source = if source = "" then None else Some (Utf8.normalize source);
                })
              in
              r :: accu
          | None -> accu
        in
        match rp.Def.r_moth with
        | Some ip ->
          let p = Gwdb.poi base ip in
          let mother = pers_to_piqi_person_link conf base p in
          let rpt_type =
            match rp.Def.r_type with
            | Adoption -> `rpt_adoption_mother
            | Recognition -> `rpt_recognition_mother
            | CandidateParent -> `rpt_candidate_parent_mother
            | GodParent -> `rpt_god_parent_mother
            | FosterParent -> `rpt_foster_parent_mother
          in
          let r =
            Api_saisie_write_piqi.Relation_parent.({
                rpt_type = rpt_type;
                person = Some mother;
                source = if source = "" then None else Some (Utf8.normalize source);
              })
          in
          r :: accu
        | None -> accu)
      (Gwdb.get_rparents p) []
  in
  let access =
    match Gwdb.get_access p with
    | IfTitles -> `access_iftitles
    | Public -> `access_public
    | Private -> `access_private
  in
  let parents =
    match Gwdb.get_parents p with
    | Some ifam -> Some (Int32.of_string (Gwdb.string_of_ifam ifam))
    | None -> None
  in
  let families =
    Mutil.array_to_list_map (fun x -> Int32.of_string @@ Gwdb.string_of_ifam x) (Gwdb.get_family p)
  in
  let is_contemporary = Geneweb.GWPARAM.is_contemporary conf base p in
  {
    Api_saisie_write_piqi.Person.digest = digest;
    index = index;
    sex = sex;
    lastname = Utf8.normalize surname;
    firstname = Utf8.normalize first_name;
    occ = occ;
    public_name = if publicname = "" then None else Some (Utf8.normalize publicname);
    aliases = List.map Utf8.normalize aliases;
    qualifiers = List.map Utf8.normalize qualifiers;
    firstname_aliases = List.map Utf8.normalize firstname_aliases;
    surname_aliases = List.map Utf8.normalize surname_aliases;
    image = Option.map Utf8.normalize image;
    death_type = death_type;
    occupation = if occupation = "" then None else Some (Utf8.normalize occupation);
    psources = if psources = "" then None else Some (Utf8.normalize psources);
    notes = if notes = "" then None else Some (Utf8.normalize notes);
    titles = titles;
    pevents = pevents;
    related = related;
    rparents = rparents;
    access = access;
    parents = parents;
    families = families;
    create_link = create_link;
    is_contemporary;
    name_is_hidden = Geneweb.NameDisplay.is_hidden conf base p;
    name_is_restricted = Geneweb.NameDisplay.is_restricted conf base p;
  }

let fam_to_piqi_mod_family conf base ifam fam =
  let digest = "" in
  let index = Int32.of_string (Gwdb.string_of_ifam ifam) in
  let fevents =
    List.map
      (fun evt ->
         let (fevent_type, event_perso) =
           match Gwdb.get_fevent_name evt with
           | Efam_Marriage -> (Some `efam_marriage, None)
           | Efam_NoMarriage -> (Some `efam_no_marriage, None)
           | Efam_NoMention -> (Some `efam_no_mention, None)
           | Efam_Engage -> (Some `efam_engage, None)
           | Efam_Divorce -> (Some `efam_divorce, None)
           | Efam_Separated -> (Some `efam_separated, None)
           | Efam_Annulation -> (Some `efam_annulation, None)
           | Efam_MarriageBann -> (Some `efam_marriage_bann, None)
           | Efam_MarriageContract -> (Some `efam_marriage_contract, None)
           | Efam_MarriageLicense -> (Some `efam_marriage_license, None)
           | Efam_PACS -> (Some `efam_pacs, None)
           | Efam_Residence -> (Some `efam_residence, None)
           | Efam_Name n -> (None, Some (Utf8.normalize @@ Gwdb.sou base n))
         in
         let date =
           match Date.od_of_cdate (Gwdb.get_fevent_date evt) with
           | Some d -> Some (piqi_date_of_date d)
           | None -> None
         in
         let place = Gwdb.sou base (Gwdb.get_fevent_place evt) in
         let reason = None in
         let note = Gwdb.sou base (Gwdb.get_fevent_note evt) in
         let src = Gwdb.sou base (Gwdb.get_fevent_src evt) in
         let witnesses =
           Mutil.array_to_list_map
             (fun (ip, wk, wnote) ->
                let witness_type = Api_util.piqi_of_witness_kind wk in
                let p = Gwdb.poi base ip in
                let person_link = pers_to_piqi_person_link conf base p in
                let witness_note = Utf8.normalize @@ Gwdb.sou base wnote in
                let witness_note = if witness_note = "" then None else Some witness_note in
                Api_saisie_write_piqi.Witness.{ witness_type; person = Some person_link ; witness_note})
             (Gwdb.get_fevent_witnesses_and_notes evt)
         in
         {
           Api_saisie_write_piqi.Fevent.fevent_type = fevent_type;
           date = date;
           place = if place = "" then None else Some (Utf8.normalize place);
           reason = reason;
           note = if note = "" then None else Some (Utf8.normalize note);
           src = if src = "" then None else Some (Utf8.normalize src);
           witnesses = witnesses;
           event_perso = event_perso;
         })
      (Geneweb.Event.sort_events
         (fun e -> Geneweb.Event.Fevent (Gwdb.get_fevent_name e))
         (fun e -> Gwdb.get_fevent_date e)
         (Gwdb.get_fevents fam)
      )
  in
  let fsources = Gwdb.sou base (Gwdb.get_fsources fam) in
  let origin_file = Gwdb.sou base (Gwdb.get_origin_file fam) in
  let comment = Gwdb.sou base (Gwdb.get_comment fam) in
  let father = Gwdb.poi base (Gwdb.get_father fam) in
  let father = pers_to_piqi_mod_person conf base father in
  let mother = Gwdb.poi base (Gwdb.get_mother fam) in
  let mother = pers_to_piqi_mod_person conf base mother in
  let children =
    Mutil.array_to_list_map
      (fun ip -> pers_to_piqi_person_link conf base @@ Gwdb.poi base ip)
      (Gwdb.get_children fam)
  in
  (* Compatibilité avec GeneWeb. *)
  let old_witnesses =
    Mutil.array_to_list_map (fun x -> Int32.of_string @@ Gwdb.string_of_iper x) (Gwdb.get_witnesses fam)
  in
  {
    Api_saisie_write_piqi.Family.digest = digest;
    index = index;
    fevents = fevents;
    fsources = if fsources = "" then None else Some (Utf8.normalize fsources);
    comment = if comment = "" then None else Some (Utf8.normalize comment);
    origin_file = if origin_file = "" then None else Some (Utf8.normalize origin_file);
    father = father;
    mother = mother;
    children = children;
    old_witnesses = old_witnesses;
  }


let piqi_mod_person_of_person_start conf base start_p =
  let p = Gwdb.empty_person base (Gwdb.dummy_iper) in
  let mod_p = pers_to_piqi_mod_person conf base p in
  (* Les index négatifs ne marchent pas. *)
  mod_p.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
  mod_p.Api_saisie_write_piqi.Person.lastname <- start_p.Api_piqi.Person_start.lastname;
  mod_p.Api_saisie_write_piqi.Person.firstname <- start_p.Api_piqi.Person_start.firstname;
  mod_p.Api_saisie_write_piqi.Person.sex <- start_p.Api_piqi.Person_start.sex;
  (* Par défaut, les access sont en Private, on passe en Iftitles. *)
  mod_p.Api_saisie_write_piqi.Person.access <- `access_iftitles;
  let birth_date =
    match start_p.Api_piqi.Person_start.birth_date_year with
    | Some y ->
        let y = Int32.to_int y in
        if y > 0 then
          (match start_p.Api_piqi.Person_start.birth_date_month with
           | Some m ->
               let m = Int32.to_int m in
               (match start_p.Api_piqi.Person_start.birth_date_day with
               | Some d ->
                   let d = Int32.to_int d in
                   let dmy =
                     {Date.day = d; month = m; year = y; prec = Sure; delta = 0}
                   in
                   Some (Date.Dgreg (dmy, Date.Dgregorian))
               | None ->
                   let dmy =
                     {Date.day = 0; month = m; year = y; prec = Sure; delta = 0}
                   in
                   Some (Date.Dgreg (dmy, Date.Dgregorian)))
           | None ->
               let dmy =
                 {Date.day = 0; month = 0; year = y; prec = Sure; delta = 0}
               in
               Some (Date.Dgreg (dmy, Date.Dgregorian)))
        else
          None
    | None -> None
  in
  let birth_date =
    match birth_date with
    | Some d -> Some (piqi_date_of_date d)
    | None -> None
  in
  let birth =
    {
      Api_saisie_write_piqi.Pevent.pevent_type = Some `epers_birth;
      date = birth_date;
      place = None;
      reason = None;
      note = None;
      src = None;
      witnesses = [];
      event_perso = None;
    }
  in
  mod_p.Api_saisie_write_piqi.Person.pevents <- [birth];
  mod_p


(**/**) (* Famille vide. *)


let piqi_empty_family conf base ifam =
  let father = Gwdb.empty_person base (Gwdb.dummy_iper) in
  let mother = Gwdb.empty_person base (Gwdb.dummy_iper) in
  let father = pers_to_piqi_mod_person conf base father in
  let mother = pers_to_piqi_mod_person conf base mother in
  (* Les index négatifs ne marchent pas ! *)
  father.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
  mother.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
  (* Par défaut, les access sont en Private, on passe en Iftitles. *)
  father.Api_saisie_write_piqi.Person.access <- `access_iftitles;
  mother.Api_saisie_write_piqi.Person.access <- `access_iftitles;
  let fevents =
    let evt =
      {
        Api_saisie_write_piqi.Fevent.fevent_type = Some `efam_marriage;
        date = None;
        place = None;
        reason = None;
        note = None;
        src = None;
        witnesses = [];
        event_perso = None;
      }
    in
    [evt]
  in
  {
    Api_saisie_write_piqi.Family.digest = "";
    index = Int32.of_string (Gwdb.string_of_ifam ifam);
    fevents = fevents;
    fsources = None;
    comment = None;
    origin_file = None;
    father = father;
    mother = mother;
    children = [];
    old_witnesses = [];
  }

let reconstitute_somebody base person =
  let create_link = person.Api_saisie_write_piqi.Person_link.create_link in
  let (fn, sn, occ, create, force_create) = match create_link with
    | `link ->
      let ip = Gwdb.iper_of_string @@ Int32.to_string person.Api_saisie_write_piqi.Person_link.index in
      let p = Gwdb.poi base ip in
      let fn = Gwdb.sou base (Gwdb.get_first_name p) in
      let sn = Gwdb.sou base (Gwdb.get_surname p) in
      let occ = Gwdb.get_occ p in
      (fn, sn, occ, Geneweb.Update.Link, false)
    | `create | `create_default_occ as create_link ->
      let sex =
        match person.Api_saisie_write_piqi.Person_link.sex with
          | `male -> Def.Male
          | `female -> Def.Female
          | `unknown -> Def.Neuter
      in
      let fn = person.Api_saisie_write_piqi.Person_link.firstname in
      let sn = person.Api_saisie_write_piqi.Person_link.lastname in
      let (occ, force_create) = match create_link with
        | `create_default_occ ->
          let occurrence_number, force_create =
            match person.Api_saisie_write_piqi.Person_link.occ with
             | Some occ -> (Int32.to_int occ, false)
             | None -> (0, false)
          in
          reserve_occurrence_number
            ~first_name:fn ~surname:sn occurrence_number;
          (occurrence_number, force_create)
        | `create ->
          let occ = find_free_occ ~base ~first_name:fn ~surname:sn in
          (occ, true)
      in
      (* Update the person because if we want to find it, we have to know its occ. *)
      let () =
        match create_link with
        | `create_default_occ | `link -> ()
        | `create ->
           if occ = 0 then person.Api_saisie_write_piqi.Person_link.occ <- None
           else person.Api_saisie_write_piqi.Person_link.occ <- Some (Int32.of_int occ)
      in
      (fn, sn, occ, Geneweb.Update.Create (sex, None), force_create)
  in
  let (fn, sn) =
    (* If there are forbidden characters, delete them. *)
    let contain_fn = String.contains fn in
    let contain_sn = String.contains sn in
    if (List.exists contain_fn Name.forbidden_char)
    || (List.exists contain_sn Name.forbidden_char)
    then (Name.purge fn, Name.purge sn)
    else (fn, sn)
  in
  {first_name = fn;
   surname = sn;
   occurrence_number = occ;
   kind = create;
   force = force_create}

let to_update_key person_update =
  (person_update.first_name,
   person_update.surname,
   person_update.occurrence_number,
   person_update.kind,
   "")
