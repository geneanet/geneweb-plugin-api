(**/**) (* Fonctions pour l'auto-completion. *)


let print_auto_complete assets conf base =
  let params = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_auto_complete in
  let s = params.Api_saisie_write_piqi.Auto_complete.input in
  let max_res = Int32.to_int params.Api_saisie_write_piqi.Auto_complete.limit in
  let mode = params.Api_saisie_write_piqi.Auto_complete.field in
  let place_mode = params.Api_saisie_write_piqi.Auto_complete.place_field in
  let list =
    if Gwdb.nb_of_persons base > Caches.node_threshold then begin
      if not (Api_saisie_autocomplete.has_cache conf mode) then
        Api_saisie_autocomplete.write_caches conf base;
      let cache = Api_saisie_autocomplete.get_list_from_cache conf mode max_res s in
      let ini = Name.lower @@ Ext_string.tr '_' ' ' s in
      match mode with
      | `place | `source | `lastname | `firstname ->
        Api_search.complete_with_dico assets conf (ref @@ List.length cache) max_res place_mode ini cache
      | `occupation ->
        Api_search.complete_with_dico assets conf (ref @@ List.length cache) max_res (Some `profession) ini cache
    end
    else
      Api_search.search_auto_complete assets conf base mode place_mode max_res s
  in
  let result = { Api_saisie_write_piqi.Auto_complete_result. result = list } in
  let data = Api_saisie_write_piqi_ext.gen_auto_complete_result result in
  Api_util.print_result conf data


let print_person_search_list conf base =
  let params = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_person_search_list_params in
  let surname = params.Api_saisie_write_piqi.Person_search_list_params.lastname in
  let first_name = params.Api_saisie_write_piqi.Person_search_list_params.firstname in
  let max_res = Int32.to_int params.Api_saisie_write_piqi.Person_search_list_params.limit in
  let list =
    Api_search.search_person_list base surname first_name
  in
  let list =
    List.sort
      (fun ip1 ip2 ->
        let p1 = Gwdb.poi base ip1 in
        let p2 = Gwdb.poi base ip2 in
        let fn1 = Gwdb.sou base (Gwdb.get_first_name p1) in
        let sn1 = Gwdb.sou base (Gwdb.get_surname p1) in
        let fn2 = Gwdb.sou base (Gwdb.get_first_name p2) in
        let sn2 = Gwdb.sou base (Gwdb.get_surname p2) in
        let cmp_sn = Utf8.alphabetic_order sn1 sn2 in
        if cmp_sn = 0 then
          let cmp_fn = Utf8.alphabetic_order fn1 fn2 in
          if cmp_fn = 0 then
            (match
              (Date.od_of_cdate (Gwdb.get_birth p1),
               Date.od_of_cdate (Gwdb.get_birth p2))
             with
             | (Some d1, Some d2) -> Date.compare_date d1 d2
             | (Some _, _) -> -1
             | (_, Some _) -> 1
             | (_, _) -> 0)
          else cmp_fn
        else cmp_sn)
      list
  in
  (* On préfère limiter la liste ici, même si on perd un peu en performance. *)
  let list = Ext_list.take list max_res in
  let () = Geneweb.SosaCache.build_sosa_ht conf base in
  let list =
    List.map
      (fun ip ->
        let p = Gwdb.poi base ip in
        Api_update_util.pers_to_piqi_person_search conf base p)
      list
  in
  let result = Api_saisie_write_piqi.Person_search_list.({ persons = list; }) in
  let data = Api_saisie_write_piqi_ext.gen_person_search_list result in
  Api_util.print_result conf data


let print_person_search_info conf base =
  let params = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_index_person in
  let ip = Gwdb.iper_of_string @@ Int32.to_string params.Api_saisie_write_piqi.Index_person.index in
  let p = Gwdb.poi base ip in
  let pers = Api_update_util.pers_to_piqi_person_search_info conf base p in
  let data = Api_saisie_write_piqi_ext.gen_person_search_info pers in
  Api_util.print_result conf data


(**/**) (* Configuration pour la saisie. *)

let print_config conf =
  let transl_cal =
    List.map
      (fun cal ->
        let (pos, sval) =
          match cal with
          | `gregorian ->
              (cal, Geneweb.Util.transl_nth conf "gregorian/julian/french/hebrew" 0)
          | `julian ->
              (cal, Geneweb.Util.transl_nth conf "gregorian/julian/french/hebrew" 1)
          | `french ->
              (cal, Geneweb.Util.transl_nth conf "gregorian/julian/french/hebrew" 2)
          | `hebrew ->
              (cal, Geneweb.Util.transl_nth conf "gregorian/julian/french/hebrew" 3)
        in
        Api_saisie_write_piqi.Transl_calendar.({pos = pos; sval = sval;}))
      [ `gregorian; `julian; `french; `hebrew ]
  in
  let transl_cal = Api_saisie_write_piqi.Config_transl_calendar.({msg = transl_cal;}) in
  let transl_wit =
    List.map (fun wk ->
        let pos = Api_util.piqi_of_witness_kind wk in
        let sval =
          let open Api_util in
          !! (Api_util.translate_witness conf wk)
        in
        Api_saisie_write_piqi.Transl_witness_type.({pos = pos; sval = sval;}))
      Api_util.witness_kinds
  in
  let transl_wit = Api_saisie_write_piqi.Config_transl_witness_type.({msg = transl_wit;}) in
  let transl_prec =
    List.map
      (fun prec ->
        let (pos, sval) =
          match prec with
          | `sure -> (prec, Geneweb.Util.transl conf "exact")
          | `about -> (prec, Geneweb.Util.transl conf "about (date)")
          | `maybe -> (prec, Geneweb.Util.transl conf "possibly (date)")
          | `before -> (prec, Geneweb.Util.transl conf "before (date)")
          | `after -> (prec, Geneweb.Util.transl conf "after (date)")
          | `oryear -> (prec, Geneweb.Util.transl conf "or")
          | `yearint -> (prec, Geneweb.Util.transl conf "between (date)")
        in
        Api_saisie_write_piqi.Transl_precision.({pos = pos; sval = sval;}))
      [ `sure; `about; `maybe; `before; `after; `oryear; `yearint ]
  in
  let transl_prec = Api_saisie_write_piqi.Config_transl_precision.({msg = transl_prec;}) in
  let transl_death =
    List.map
      (fun death ->
        let (pos, sval) =
          match death with
          | `not_dead -> (death, Geneweb.Util.transl conf "alive")
          | `dead -> (death, Geneweb.Util.transl conf "died")
          | `dead_young -> (death, Geneweb.Util.transl conf "died young")
          | `dont_know_if_dead -> (death, Geneweb.Util.transl conf "api_dont_know_if_dead")
          | `of_course_dead -> (death, Geneweb.Util.transl conf "of course dead")
          | _ -> failwith "transl_death"
        in
        Api_saisie_write_piqi.Transl_death_type.({pos = pos; sval = sval;}))
      [ `not_dead; `dead; `dead_young; `dont_know_if_dead; `of_course_dead ]
  in
  let transl_death = Api_saisie_write_piqi.Config_transl_death_type.({msg = transl_death;}) in
  let transl_rel =
    List.map
      (fun rel ->
        let (pos, sval) =
          match rel with
          | `rpt_adoption_father ->
              (rel, Geneweb.Util.transl_nth conf "adoptive father/adoptive mother/adoptive parents" 0)
          | `rpt_adoption_mother ->
              (rel, Geneweb.Util.transl_nth conf "adoptive father/adoptive mother/adoptive parents" 1)
          | `rpt_recognition_father ->
              (rel, Geneweb.Util.transl_nth conf "recognizing father/recognizing mother/recognizing parents" 0)
          | `rpt_recognition_mother ->
              (rel, Geneweb.Util.transl_nth conf "recognizing father/recognizing mother/recognizing parents" 1)
          | `rpt_candidate_parent_father ->
              (rel, Geneweb.Util.transl_nth conf "candidate father/candidate mother/candidate parents" 0)
          | `rpt_candidate_parent_mother ->
              (rel, Geneweb.Util.transl_nth conf "candidate father/candidate mother/candidate parents" 1)
          | `rpt_god_parent_father ->
              (rel, Geneweb.Util.transl_nth conf "godfather/godmother/godparents" 0)
          | `rpt_god_parent_mother ->
              (rel, Geneweb.Util.transl_nth conf "godfather/godmother/godparents" 1)
          | `rpt_foster_parent_father ->
              (rel, Geneweb.Util.transl_nth conf "foster father/foster mother/foster parents" 0)
          | `rpt_foster_parent_mother ->
              (rel, Geneweb.Util.transl_nth conf "foster father/foster mother/foster parents" 1)
        in
        Api_saisie_write_piqi.Transl_relation_parent_type.({pos = pos; sval = sval;}))
      [ `rpt_adoption_father; `rpt_adoption_mother;
        `rpt_recognition_father; `rpt_recognition_mother;
        `rpt_candidate_parent_father; `rpt_candidate_parent_mother;
        `rpt_god_parent_father; `rpt_god_parent_mother;
        `rpt_foster_parent_father; `rpt_foster_parent_mother ]
  in
  let transl_rel =
    Api_saisie_write_piqi.Config_transl_relation_parent_type.({msg = transl_rel;})
  in
  let transl_fevent =
    List.map
      (fun evt ->
        let (pos, sval) =
          ( Api_piqi_util.piqi_fevent_name_of_fevent_name evt
          , Geneweb.Util.string_of_fevent_name_without_base conf evt )
        in
        let open Api_util in
        Api_saisie_write_piqi.Transl_fevent_name.({
          pos = pos;
          sval = !!(sval);
        }))
      [ Def.Efam_Marriage; Def.Efam_NoMarriage; Def.Efam_NoMention; Def.Efam_Engage;
        Def.Efam_Divorce ; Def.Efam_Separated;
        Def.Efam_Annulation; Def.Efam_MarriageBann; Def.Efam_MarriageContract;
        Def.Efam_MarriageLicense; Def.Efam_PACS; Def.Efam_Residence ]
  in
  let transl_fevent =
    Api_saisie_write_piqi.Config_transl_fevent_name.({msg = transl_fevent;})
  in
  (* On tri les évènements. Les 4 principaux en premier, *)
  (* les autres et les LDS en derniers.                  *)
  let transl_pevent_prim =
    List.map
      (fun evt ->
        let (pos, sval) =
          ( Api_piqi_util.piqi_pevent_name_of_pevent_name evt
          , Geneweb.Util.string_of_pevent_name_without_base conf evt )
        in
        let open Api_util in
        Api_saisie_write_piqi.Transl_pevent_name.({
          pos = pos;
          sval = !!(sval);
        }))
      [ Def.Epers_Birth; Def.Epers_Baptism; Def.Epers_Death; Def.Epers_Burial ]
  in
  let transl_pevent_sec =
    List.map
      (fun evt ->
        let (pos, sval) =
          ( Api_piqi_util.piqi_pevent_name_of_pevent_name evt
          , Geneweb.Util.string_of_pevent_name_without_base conf evt )
        in
        let open Api_util in
        Api_saisie_write_piqi.Transl_pevent_name.({
          pos = pos;
          sval = !!(sval);
        }))
      [ Def.Epers_Accomplishment; Def.Epers_Acquisition; Def.Epers_Adhesion;
        Def.Epers_BarMitzvah; Def.Epers_BatMitzvah; Def.Epers_Benediction; Def.Epers_Cremation;
        Def.Epers_ChangeName; Def.Epers_Circumcision; Def.Epers_Confirmation;
        Def.Epers_Decoration; Def.Epers_DemobilisationMilitaire; Def.Epers_Diploma;
        Def.Epers_Distinction; Def.Epers_Dotation; Def.Epers_Education; Def.Epers_Election;
        Def.Epers_Emigration; Def.Epers_Excommunication; Def.Epers_FirstCommunion;
        Def.Epers_Funeral; Def.Epers_Graduate; Def.Epers_Hospitalisation;
        Def.Epers_Illness; Def.Epers_Immigration; Def.Epers_ListePassenger;
        Def.Epers_MilitaryDistinction; Def.Epers_MilitaryPromotion; Def.Epers_MilitaryService;
        Def.Epers_MobilisationMilitaire; Def.Epers_Naturalisation; Def.Epers_Occupation;
        Def.Epers_Ordination; Def.Epers_Property; Def.Epers_Recensement; Def.Epers_Residence;
        Def.Epers_Retired; Def.Epers_VenteBien; Def.Epers_Will ]
  in
  let transl_pevent_sec =
    List.sort
      (fun msg1 msg2 ->
        Utf8.alphabetic_order
          msg1.Api_saisie_write_piqi.Transl_pevent_name.sval
          msg2.Api_saisie_write_piqi.Transl_pevent_name.sval)
      transl_pevent_sec
  in
  let transl_pevent_LDS =
    List.map
      (fun evt ->
        let (pos, sval) =
          ( Api_piqi_util.piqi_pevent_name_of_pevent_name evt
          , Geneweb.Util.string_of_pevent_name_without_base conf evt )
        in
        let open Api_util in
        Api_saisie_write_piqi.Transl_pevent_name.({
          pos = pos;
          sval = !!(sval);
        }))
      [ Def.Epers_BaptismLDS; Def.Epers_ConfirmationLDS; Def.Epers_DotationLDS;
        Def.Epers_FamilyLinkLDS; Def.Epers_ScellentChildLDS; Def.Epers_ScellentParentLDS;
        Def.Epers_ScellentSpouseLDS ]
  in
  let transl_pevent_LDS =
    List.sort
      (fun msg1 msg2 ->
        Utf8.alphabetic_order
          msg1.Api_saisie_write_piqi.Transl_pevent_name.sval
          msg2.Api_saisie_write_piqi.Transl_pevent_name.sval)
      transl_pevent_LDS
  in
  let transl_pevent = transl_pevent_prim @ transl_pevent_sec @ transl_pevent_LDS in
  let transl_pevent =
    Api_saisie_write_piqi.Config_transl_pevent_name.({msg = transl_pevent;})
  in
  let transl_access =
    List.map
      (fun access ->
        let (pos, sval) =
          match access with
          | Def.IfTitles -> (`access_iftitles, Geneweb.Util.transl_nth conf "iftitles/public/private" 0)
          | Def.Public -> (`access_public, Geneweb.Util.transl_nth conf "iftitles/public/private" 1)
          | Def.Private -> (`access_private, Geneweb.Util.transl_nth conf "iftitles/public/private" 2)
        in
        Api_saisie_write_piqi.Transl_access.({
          pos = pos;
          sval = sval;
        }))
      [ Def.IfTitles; Def.Public; Def.Private ]
  in
  let transl_access = Api_saisie_write_piqi.Config_transl_access.({msg = transl_access;}) in
  let transl_warning =
    List.map
      (fun warn ->
        let (pos, sval) =
          match warn with
          (* erreur JS *)
          | `empty_index -> (warn, Geneweb.Util.transl conf "index required")
          | `empty_surname -> (warn, Geneweb.Util.transl conf "surname missing")
          | `empty_first_name -> (warn, Geneweb.Util.transl conf "first name missing")
          | `empty_sex -> (warn, Geneweb.Util.transl conf "sex required")
          | `required_field -> (warn, Geneweb.Util.transl conf "field required")
          | `birth_date_after_event -> (warn, Geneweb.Util.transl conf "birth date after event")
          | `death_date_before_event -> (warn, Geneweb.Util.transl conf "death date before event")
        in
        Api_saisie_write_piqi.Transl_update_warning_js.({
          pos = pos;
          sval = sval;
        }))
      [ `empty_index; `empty_surname; `empty_first_name;
        `empty_sex; `required_field; `birth_date_after_event;
        `death_date_before_event ]
  in
  let transl_warning = Api_saisie_write_piqi.Config_transl_update_warning_js.({msg = transl_warning;}) in
  let transl_short_greg_month =
    List.map
      (fun month ->
        let (pos, sval) =
          match month with
          | `janv -> (month, Geneweb.Util.transl_nth conf "(short month)" 0)
          | `fevr -> (month, Geneweb.Util.transl_nth conf "(short month)" 1)
          | `mars -> (month, Geneweb.Util.transl_nth conf "(short month)" 2)
          | `avr -> (month, Geneweb.Util.transl_nth conf "(short month)" 3)
          | `mai -> (month, Geneweb.Util.transl_nth conf "(short month)" 4)
          | `juin -> (month, Geneweb.Util.transl_nth conf "(short month)" 5)
          | `juil -> (month, Geneweb.Util.transl_nth conf "(short month)" 6)
          | `aout -> (month, Geneweb.Util.transl_nth conf "(short month)" 7)
          | `sept -> (month, Geneweb.Util.transl_nth conf "(short month)" 8)
          | `oct -> (month, Geneweb.Util.transl_nth conf "(short month)" 9)
          | `nov -> (month, Geneweb.Util.transl_nth conf "(short month)" 10)
          | `dec -> (month, Geneweb.Util.transl_nth conf "(short month)" 11)
        in
        Api_saisie_write_piqi.Transl_short_greg_month.({
          pos = pos;
          sval = sval;
        }))
      [ `janv; `fevr; `mars; `avr; `mai; `juin;
        `juil; `aout; `sept; `oct; `nov; `dec ]
  in
  let transl_short_greg_month = Api_saisie_write_piqi.Config_transl_short_greg_month.({msg = transl_short_greg_month;}) in
  let transl_french_month =
    List.map
      (fun month ->
        let (pos, sval) =
          match month with
          | `vendemiaire -> (month, Geneweb.Util.transl_nth conf "(french revolution month)" 0)
          | `brumaire -> (month, Geneweb.Util.transl_nth conf "(french revolution month)" 1)
          | `frimaire -> (month, Geneweb.Util.transl_nth conf "(french revolution month)" 2)
          | `nivose -> (month, Geneweb.Util.transl_nth conf "(french revolution month)" 3)
          | `pluviose -> (month, Geneweb.Util.transl_nth conf "(french revolution month)" 4)
          | `ventose -> (month, Geneweb.Util.transl_nth conf "(french revolution month)" 5)
          | `germinal -> (month, Geneweb.Util.transl_nth conf "(french revolution month)" 6)
          | `floreal -> (month, Geneweb.Util.transl_nth conf "(french revolution month)" 7)
          | `prairial -> (month, Geneweb.Util.transl_nth conf "(french revolution month)" 8)
          | `messidor -> (month, Geneweb.Util.transl_nth conf "(french revolution month)" 9)
          | `thermidor -> (month, Geneweb.Util.transl_nth conf "(french revolution month)" 10)
          | `fructidor -> (month, Geneweb.Util.transl_nth conf "(french revolution month)" 11)
          | `complementaire -> (month, Geneweb.Util.transl_nth conf "(french revolution month)" 12)
        in
        Api_saisie_write_piqi.Transl_french_month.({
          pos = pos;
          sval = sval;
        }))
      [ `vendemiaire; `brumaire; `frimaire; `nivose; `pluviose; `ventose;
        `germinal; `floreal; `prairial; `messidor; `thermidor; `fructidor;
        `complementaire ]
  in
  let transl_french_month = Api_saisie_write_piqi.Config_transl_french_month.({msg = transl_french_month;}) in
  let transl_hebrew_month =
    List.map
      (fun month ->
        let (pos, sval) =
          match month with
          | `tichri -> (month, Geneweb.Util.transl_nth conf "(hebrew month)" 0)
          | `marhechvan -> (month, Geneweb.Util.transl_nth conf "(hebrew month)" 1)
          | `kislev -> (month, Geneweb.Util.transl_nth conf "(hebrew month)" 2)
          | `tevet -> (month, Geneweb.Util.transl_nth conf "(hebrew month)" 3)
          | `chevat -> (month, Geneweb.Util.transl_nth conf "(hebrew month)" 4)
          | `adar_1 -> (month, Geneweb.Util.transl_nth conf "(hebrew month)" 5)
          | `adar_2 -> (month, Geneweb.Util.transl_nth conf "(hebrew month)" 6)
          | `nissan -> (month, Geneweb.Util.transl_nth conf "(hebrew month)" 7)
          | `iyar -> (month, Geneweb.Util.transl_nth conf "(hebrew month)" 8)
          | `sivan -> (month, Geneweb.Util.transl_nth conf "(hebrew month)" 9)
          | `tamouz -> (month, Geneweb.Util.transl_nth conf "(hebrew month)" 10)
          | `av -> (month, Geneweb.Util.transl_nth conf "(hebrew month)" 11)
          | `eloul -> (month, Geneweb.Util.transl_nth conf "(hebrew month)" 12)
        in
        Api_saisie_write_piqi.Transl_hebrew_month.({
          pos = pos;
          sval = sval;
        }))
      [ `tichri; `marhechvan; `kislev; `tevet; `chevat; `adar_1;
        `adar_2; `nissan; `iyar; `sivan; `tamouz; `av; `eloul ]
  in
  let transl_hebrew_month = Api_saisie_write_piqi.Config_transl_hebrew_month.({msg = transl_hebrew_month;}) in
  let (gwf_place_format, gwf_place_format_placeholder) =
    match List.assoc_opt "places_format" conf.Geneweb.Config.base_env with
    | Some s ->
        let placeholder =
          (try
             List.fold_right
               (fun s accu ->
                  match s with
                  | "Subdivision" -> accu
                  | "Town" -> (Geneweb.Util.transl conf "town") :: accu
                  | "Area code" -> (Geneweb.Util.transl conf "area code") :: accu
                  | "County" -> (Geneweb.Util.transl conf "county") :: accu
                  | "Region" -> (Geneweb.Util.transl conf "region") :: accu
                  | "Country" -> (Geneweb.Util.transl conf "country") :: accu
                  | _ -> raise Not_found)
               (String.split_on_char ',' s) []
           with Not_found -> [])
        in
        let placeholder = String.concat ", " placeholder in
        (* On ajoute les lieux-dit. *)
        let placeholder =
          match String.split_on_char ',' s with
          | "Subdivision" :: _ ->
            "[" ^ (Geneweb.Util.transl conf "subdivision") ^ "] - " ^ placeholder
          | _ -> placeholder
        in
        (s, placeholder)
    | None -> ("", "")
  in
  let config =
    Api_saisie_write_piqi.Config.({
      transl_cal = transl_cal;
      transl_wit = transl_wit;
      transl_prec = transl_prec;
      transl_death = transl_death;
      transl_rel = transl_rel;
      transl_fevents = transl_fevent;
      transl_pevents = transl_pevent;
      transl_access = transl_access;
      transl_warning = transl_warning;
      transl_short_greg_month = transl_short_greg_month;
      transl_french_month = transl_french_month;
      transl_hebrew_month = transl_hebrew_month;
      gwf_place_format = gwf_place_format;
      gwf_place_format_placeholder = gwf_place_format_placeholder;
    })
  in
  let data = Api_saisie_write_piqi_ext.gen_config config in
  Api_util.print_result conf data


(**/**) (* Fonctions qui calcul "l'inférence" du nom de famille. *)

type children_surname =
  | NoChild
  | NoSurname
  | Surname of string

let children_surname base fam =
  let count = ref 0 in
  let fam' =
    Array.map begin fun i ->
      let c = Gwdb.get_children @@ Gwdb.foi base i in
      count := !count + Array.length c ;
      c
    end fam
  in
  let surnames = Array.make !count "" in
  count := 0 ;
  Array.iter begin Array.iter begin fun i ->
      surnames.(!count) <- Gwdb.sou base @@ Gwdb.get_surname @@ Gwdb.poi base i ;
      incr count
    end end fam' ;
  match surnames with
  | [||] -> NoChild
  | [|x|] -> Surname x
  | a ->
    let x_crush = Name.crush_lower a.(0) in
    if Array.for_all (fun n -> Name.crush_lower n = x_crush) a
    then Surname a.(0)
    else NoSurname

let infer_surname_from_parents base surname p =
  match Gwdb.get_parents p with
  | Some ifam -> begin
      let g_fam = Gwdb.foi base ifam in
      let g_father = Gwdb.poi base (Gwdb.get_father g_fam) in
      if Name.crush_lower surname
         = Name.crush_lower (Gwdb.sou base (Gwdb.get_surname g_father))
      then surname
      else ""
    end
  | None -> ""

(** [infer_surname conf base p ifam] *)
let rec infer_surname conf base p ifam =
  let surname = Gwdb.sou base (Gwdb.get_surname p) in
  if surname = "?" then ""
  else match ifam with
    | Some ifam ->
      let ifam = Gwdb.ifam_of_string ifam in
      let fam = Gwdb.foi base ifam in
      (* if p is not in the couple, father's iper is returned by [spouse] *)
      let isp = Gutil.spouse (Gwdb.get_iper p) fam in
      let sp = Gwdb.poi base isp in
      if Gwdb.get_sex sp = Def.Male then infer_surname conf base sp None
      else ""
    | None ->
      if Gwdb.get_sex p = Def.Male && Array.length (Gwdb.get_family p) > 0
      then match children_surname base (Gwdb.get_family p) with
        | NoSurname -> ""
        | Surname s -> s
        | NoChild -> infer_surname_from_parents base surname p
      else infer_surname_from_parents base surname p

let infer_death conf base p =
  Api_util.piqi_death_type_of_death (Geneweb.Update.infer_death conf base p)

let empty_death_pevent () =
  { Api_saisie_write_piqi.Pevent.pevent_type = Some `epers_death;
    date = None;
    place = None;
    reason = None;
    note = None;
    src = None;
    witnesses = [];
    event_perso = None;
  }

(**/**) (* Fonctions qui renvoie le ModificationStatus. *)
let print_someone base p =
  let open Api_util in
  !!  (Geneweb.Util.escape_html @@ Gwdb.sou base (Gwdb.get_first_name p) ^ " " ^ Gwdb.sou base (Gwdb.get_surname p))

let print_someone_dates conf base p =
  let open Api_util in
  print_someone base p ^ " " ^ !!(Geneweb.DateDisplay.short_dates_text conf base p)

let merge_dup_link conf iper txt =
  let iper_s = Gwdb.string_of_iper iper in
  let henv = ["i", Adef.encoded iper_s;
              "ip", Adef.encoded iper_s;
              "m", Adef.encoded "MRG_DUP";]
  in
  let open Api_util in
  "<a href=" ^ !!(Geneweb.Util.commd {conf with henv})  ^ ">"
  ^ txt
  ^ "</a>"

let possible_family_dup conf base f1 =
  let f = Gwdb.foi base f1 in
  let w =
    Printf.sprintf
      (Geneweb.Util.fcapitale (Geneweb.Util.ftransl conf "%s and %s have several unions"))
      (print_someone base @@ Gwdb.poi base @@ Gwdb.get_father f)
      (print_someone base @@ Gwdb.poi base @@ Gwdb.get_mother f)
  in
  let link = merge_dup_link conf (Gwdb.get_father f)
               (Geneweb.Util.transl conf "click here to merge these unions" |> Utf8.capitalize_fst)
  in
  w ^ ". " ^ link

let possible_family_dup_homonmous conf base fam p =
  let f = Gwdb.foi base fam in
  let father = Gwdb.get_father f in
  let mother = Gwdb.get_mother f in
  let hom, curr  =
    if Gwdb.eq_iper father (Gwdb.get_iper p) then mother, father
    else father, mother
  in
  let w =
    Printf.sprintf
      (Geneweb.Util.fcapitale (Geneweb.Util.ftransl conf "%s has unions with several persons named %s"))
      (print_someone base @@ Gwdb.poi base @@ curr)
      (print_someone base @@ Gwdb.poi base @@ hom)
  in
  let txt = Geneweb.Util.transl conf "click here to merge these persons and their unions"
            |> Utf8.capitalize_fst
  in
  let link = merge_dup_link conf curr txt in
  w ^ ". " ^ link

let compute_warnings conf base resp =
  let get_pevent_name e = e.Def.epers_name in
  let get_fevent_name e = e.Def.efam_name in
  let print_someone = print_someone base in
  let print_someone_dates = print_someone_dates conf base in
  match resp with
  | Api_update_util.UpdateErrorConflict c -> (false, [], [], Some c, [], None)
  | Api_update_util.UpdateError s ->
     let open Api_util in
     (false, [!!(Geneweb.Update.string_of_error conf s)], [], None, [], None)
  | Api_update_util.UpdateSuccess (wl, ml, hr, cp) ->
      let warning =
        List.fold_right
          (fun w wl ->
            match w with
            | Geneweb.Warning.BigAgeBetweenSpouses (p1, p2, a) ->
                let w =
                  let open Api_util in
                  (Printf.sprintf
                     (Geneweb.Util.fcapitale
                        (Geneweb.Util.ftransl conf
                           "the difference of age between %t and %t is quite important"))
                     (fun _ -> print_someone p1)
                     (fun _ -> print_someone p2))
                  ^ ": " ^ !!(Geneweb.DateDisplay.string_of_age conf a)
                in
                w :: wl
            | BirthAfterDeath p ->
                let w =
                Printf.sprintf
                  (Geneweb.Util.ftransl conf "%t died before his/her birth")
                  (fun _ -> print_someone_dates p)
                in
                w :: wl
            | ChangedOrderOfChildren _ -> wl
                (* On ignore les messages de changement d'ordre. *)
            | ChangedOrderOfMarriages _ -> wl
                (* On ignore les messages de changement d'ordre. *)
            | ChangedOrderOfFamilyEvents _ -> wl
                (* On ignore les messages de changement d'ordre. *)
            | ChangedOrderOfPersonEvents _ -> wl
                (* On ignore les messages de changement d'ordre. *)
            | ChildrenNotInOrder _ -> wl
                (* On ignore les messages de changement d'ordre. *)
            | CloseChildren (ifam, c1, c2) ->
                let cpl = Gwdb.foi base ifam in
                let w =
                (Printf.sprintf
                   (Geneweb.Util.fcapitale
                      (Geneweb.Util.ftransl conf
                         "the following children of %t and %t are born very close"))
                   (fun _ -> print_someone (Gwdb.poi base (Gwdb.get_father cpl)))
                   (fun _ -> print_someone (Gwdb.poi base (Gwdb.get_mother cpl))))
                ^ ": " ^
                print_someone_dates c1 ^ " " ^ print_someone_dates c2
                in
                w :: wl
            | DistantChildren (ifam, p1, p2) ->
                let cpl = Gwdb.foi base ifam in
                let w =
                (Printf.sprintf
                   (Geneweb.Util.fcapitale
                      (Geneweb.Util.ftransl conf
                         "the following children of %t and %t are born very distant"))
                   (fun _ -> print_someone (Gwdb.poi base (Gwdb.get_father cpl)))
                   (fun _ -> print_someone (Gwdb.poi base (Gwdb.get_mother cpl))))
                ^ ": " ^
                print_someone_dates p1 ^ " " ^ print_someone_dates p2
                in
                w :: wl
            | DeadOld (p, a) ->
                let w =
                let open Api_util in
                print_someone p
                  ^ " " ^
                  (Geneweb.Util.transl_nth
                     conf "died at an advanced age" (Geneweb.Util.index_of_sex (Gwdb.get_sex p)))
                  ^ " " ^
                  !!(Geneweb.DateDisplay.string_of_age conf a)
                in
                w :: wl
            | DeadTooEarlyToBeFather (father, child) ->
                let w =
                Printf.sprintf
                  (Geneweb.Util.ftransl conf "\
          %t is born more than 2 years after the death of his/her father %t")
                  (fun _ -> print_someone_dates child)
                  (fun _ -> print_someone_dates father)
                in
                w :: wl
            | FEventOrder (p, e1, e2) ->
                let w =
                  let open Api_util in
                  Printf.sprintf
                    (Geneweb.Util.ftransl conf "%t's %s before his/her %s")
                    (fun _ -> print_someone_dates p)
                    !!(Geneweb.Util.string_of_fevent_name conf base (get_fevent_name e1))
                    !!(Geneweb.Util.string_of_fevent_name conf base (get_fevent_name e2))
                in
                w :: wl
            | FWitnessEventAfterDeath (p, e, _) ->
                let w =
                  let open Api_util in
                  Printf.sprintf
                    (Geneweb.Util.ftransl conf "%t witnessed the %s after his/her death")
                    (fun _ -> print_someone_dates p)
                    !!(Geneweb.Util.string_of_fevent_name conf base (get_fevent_name e))
                in
                w :: wl
            | FWitnessEventBeforeBirth (p, e, _) ->
                let w =
                  let open Api_util in
                  Printf.sprintf
                    (Geneweb.Util.ftransl conf "%t witnessed the %s before his/her birth")
                    (fun _ -> print_someone_dates p)
                    !!(Geneweb.Util.string_of_fevent_name conf base (get_fevent_name e))
                in
                w :: wl
            | IncoherentSex (p, _, _) ->
                let w =
                Printf.sprintf
                  (Geneweb.Util.fcapitale
                     (Geneweb.Util.ftransl conf "%t's sex is not coherent with his/her relations"))
                  (fun _ -> print_someone p)
                in
                w :: wl
            | IncoherentAncestorDate (anc, p) ->
                let w =
                Printf.sprintf "%s has a younger ancestor %s"
                  (print_someone p)
                  (print_someone anc)
                in
                w :: wl
            | MarriageDateAfterDeath p ->
                let w =
                Printf.sprintf
                  (Geneweb.Util.fcapitale (Geneweb.Util.ftransl conf "marriage had occurred after the death of %t"))
                  (fun _ -> print_someone_dates p)
                in
                w :: wl
            | MarriageDateBeforeBirth p ->
                let w =
                Printf.sprintf
                  (Geneweb.Util.fcapitale (Geneweb.Util.ftransl conf "marriage had occurred before the birth of %t"))
                  (fun _ -> print_someone_dates p)
                in
                w :: wl
            | MotherDeadBeforeChildBirth (mother, child) ->
                let w =
                Printf.sprintf
                  (Geneweb.Util.ftransl conf "%t is born after the death of his/her mother %t")
                  (fun _ -> print_someone_dates child)
                  (fun _ -> print_someone_dates mother)
                in
                w :: wl
            | ParentBornAfterChild (p, c) ->
                let w =
                Printf.sprintf "%s\n%s\n%s" (print_someone p)
                  (Geneweb.Util.transl conf "is born after his/her child")
                  (print_someone c)
                in
                w :: wl
            | ParentTooYoung (p, a, _) ->
                let w =
                let open Api_util in
                Printf.sprintf "%s\n%s\n" (print_someone_dates p)
                  (Geneweb.Util.transl conf "is a very young parent") ^
                Printf.sprintf "(%s)" !!(Geneweb.DateDisplay.string_of_age conf a)
                in
                w :: wl
            | PossibleDuplicateFam (f1, _) ->
               let w = possible_family_dup conf base f1 in
               w :: wl
            | PossibleDuplicateFamHomonymous (f1, _, p) ->
               let w = possible_family_dup_homonmous conf base f1 p in
               w :: wl
            | ParentTooOld (p, a, _) ->
                let w =
                let open Api_util in
                Printf.sprintf "%s\n%s\n" (print_someone p)
                  (Geneweb.Util.transl conf "is a very old parent") ^
                Printf.sprintf "(%s)" !!(Geneweb.DateDisplay.string_of_age conf a);
                in
                w :: wl
            | PEventOrder (p, e1, e2) ->
                let w =
                  let open Api_util in
                  Printf.sprintf
                    (Geneweb.Util.ftransl conf "%t's %s before his/her %s")
                    (fun _ -> print_someone_dates p)
                    !!(Geneweb.Util.string_of_pevent_name conf base (get_pevent_name e1))
                    !!(Geneweb.Util.string_of_pevent_name conf base (get_pevent_name e2))
                in
                w :: wl
            | PWitnessEventAfterDeath (p, e, _) ->
                let w =
                  let open Api_util in
                  Printf.sprintf
                    (Geneweb.Util.ftransl conf "%t witnessed the %s after his/her death")
                    (fun _ -> print_someone_dates p)
                    !!(Geneweb.Util.string_of_pevent_name conf base (get_pevent_name e))
                in
                w :: wl
            | PWitnessEventBeforeBirth (p, e, _) ->
                let w =
                  let open Api_util in
                  Printf.sprintf
                    (Geneweb.Util.ftransl conf "%t witnessed the %s before his/her birth")
                    (fun _ -> print_someone_dates p)
                    !!(Geneweb.Util.string_of_pevent_name conf base (get_pevent_name e))
                in
                w :: wl
            | TitleDatesError (p, t) ->
                let w =
                Printf.sprintf
                  (Geneweb.Util.fcapitale (Geneweb.Util.ftransl conf "%t has incorrect title dates: %t"))
                  (fun _ -> print_someone_dates p)
                  (fun _ ->
                     Printf.sprintf "%s %s %s-%s"
                       (Gwdb.sou base t.Def.t_ident) (Gwdb.sou base t.t_place)
                       (match Date.od_of_cdate t.t_date_start with
                        | Some d ->
                           let open Api_util in
                           !!(Geneweb.DateDisplay.string_of_date conf d)
                        | _ -> "" )
                       (match Date.od_of_cdate t.t_date_end with
                        | Some d ->
                           let open Api_util in
                           !!(Geneweb.DateDisplay.string_of_date conf d)
                        | _ -> "" ))
                in
                w :: wl
            | UndefinedSex p ->
                let w =
                Printf.sprintf
                  (Geneweb.Util.fcapitale (Geneweb.Util.ftransl conf "undefined sex for %t"))
                  (fun _ -> print_someone p)
                in
                w :: wl
            | YoungForMarriage (p, a, _)
            | OldForMarriage (p, a, _) ->
                let w =
                let open Api_util in
                print_someone p ^ " " ^
                  (Printf.sprintf
                     (Geneweb.Util.ftransl conf "married at age %t")
                     (fun _ -> !!(Geneweb.DateDisplay.string_of_age conf a)))
                in
                w :: wl)
          wl []
      in
      let misc =
        List.fold_right
          (fun m ml ->
            match m with
            | Geneweb.Warning.MissingSources ->
                let m = Utf8.capitalize_fst (Geneweb.Util.transl conf "missing sources") in
                m :: ml)
          ml []
      in
      (true, warning, misc, None, hr, cp)

let compute_modification_status' conf base ip ifam resp =
  let (surname, first_name, occ, index_person, surname_str, first_name_str) =
    if ip = Gwdb.dummy_iper then ("", "", None, None, None, None)
    else
      let p = Gwdb.poi base ip in
      let surname = Gwdb.sou base (Gwdb.get_surname p) in
      let first_name = Gwdb.sou base (Gwdb.get_first_name p) in
      let index_person = Some (Int32.of_string @@ Gwdb.string_of_iper ip) in
      let occ = Gwdb.get_occ p in
      let occ = if occ = 0 then None else Some (Int32.of_int occ) in
      let surname_str = Some (Gwdb.sou base (Gwdb.get_surname p)) in
      let first_name_str = Some (Gwdb.sou base (Gwdb.get_first_name p)) in
      if not (Geneweb.Util.accessible_by_key conf base p first_name surname) ||
         (surname = "" && first_name = "")
      then
        ("", "", None, index_person, surname_str, first_name_str)
      else
        (surname, first_name, occ, index_person, surname_str, first_name_str)
  in
  let sn = if surname = "" then None else Some (Name.lower surname) in
  let fn = if first_name = "" then None else Some (Name.lower first_name) in
  let index_family = if ifam = Gwdb.dummy_ifam then None else Some (Int32.of_string @@ Gwdb.string_of_ifam ifam) in
  let (is_base_updated, warnings, miscs, conflict, history_records, created_person) =
    compute_warnings conf base resp
  in
  (* Maintenant que l'on sait si tout s'est bien passé, *)
  (* on peut enfin commiter le fichier patch.           *)
  let () =
    if is_base_updated then
      begin
        List.iter (fun f -> f ()) history_records;
        Geneweb.Util.commit_patches conf base;
      end
    else ()
  in
  let response =
    {
      Api_saisie_write_piqi.Modification_status.is_base_updated = is_base_updated;
      base_warnings = warnings;
      base_miscs = miscs;
      index_person = index_person;
      lastname = surname;
      firstname = first_name;
      occ = occ;
      index_family = index_family;
      conflict = conflict;
      lastname_str = surname_str;
      firstname_str = first_name_str;
      n = sn;
      p = fn;
      created_person = Option.map (fun cp -> Api_saisie_write_piqi.Created_person.{
        n = cp.Api_update_util.n;
        p = cp.p;
        oc = cp.oc;
      }) created_person;
    }
  in
  response

let compute_modification_status conf base ip fam resp =
  let response = compute_modification_status' conf base ip fam resp in
  Api_saisie_write_piqi_ext.gen_modification_status response

(**/**) (* Fonctions d'ajout de la première personne. *)


let print_add_ind_start_ok conf base =
  let start_p = Api_util.get_params conf Api_piqi_ext.parse_person_start in
  let mod_p =
    Api_update_util.piqi_mod_person_of_person_start conf base start_p
  in
  let resp = Api_update_person.print_add conf base mod_p in
  let ref_p =
    match resp with
    | Api_update_util.UpdateError _  | Api_update_util.UpdateErrorConflict _ ->
      Api_util.empty_reference_person
    | Api_update_util.UpdateSuccess _ ->
        Geneweb.Util.commit_patches conf base;
        let ip = Gwdb.iper_of_string @@ Int32.to_string mod_p.Api_saisie_write_piqi.Person.index in
        Api_util.person_to_reference_person base @@ Gwdb.poi base ip
  in
  let data = Api_piqi_ext.gen_reference_person ref_p in
  Api_util.print_result conf data


(**/**) (* Fonctions de modification individu. *)


let print_mod_ind conf base =
  let params = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_index_person in
  let ip = Gwdb.iper_of_string @@ Int32.to_string params.Api_saisie_write_piqi.Index_person.index in
  let p = Gwdb.poi base ip in
  let mod_p = Api_update_util.pers_to_piqi_mod_person conf base p in
  let data = Api_saisie_write_piqi_ext.gen_person mod_p in
  Api_util.print_result conf data


let print_mod_ind_ok conf base =
  let mod_p = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_person in
  let resp = Api_update_person.print_mod conf base mod_p in
  let ip = Gwdb.iper_of_string @@ Int32.to_string mod_p.Api_saisie_write_piqi.Person.index in
  let data = compute_modification_status conf base ip Gwdb.dummy_ifam resp in
  Api_util.print_result conf data


let print_add_ind_ok conf base =
  let mod_p = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_person in
  let resp = Api_update_person.print_add conf base mod_p in
  let ip = Gwdb.iper_of_string @@ Int32.to_string mod_p.Api_saisie_write_piqi.Person.index in
  let data = compute_modification_status conf base ip Gwdb.dummy_ifam resp in
  Api_util.print_result conf data


(* Fonction qui calcule la personne sur laquelle on va faire la redirection. *)
(*
   Dans l'ordre :
     - sur le parent (père)
     - sur le premier enfant
     - sur le sosa
     - sur l'accueil
*)
let compute_redirect_person conf base p =
  let ip = Gwdb.get_iper p in
  match Gwdb.get_parents p with
  | Some ifam ->
      let fam = Gwdb.foi base ifam in
      let ifath = Gwdb.get_father fam in
      let imoth = Gwdb.get_mother fam in
      let father = Gwdb.poi base ifath in
      if Gwdb.sou base (Gwdb.get_surname father) = "?" &&
         Gwdb.sou base (Gwdb.get_first_name father) = "?"
      then imoth
      else ifath
  | None ->
      if Array.length (Gwdb.get_family p) > 0 then
        (* On renvoi sur le premier conjoint. *)
        let fam = Gwdb.get_family p in
        Gutil.spouse ip (Gwdb.foi base fam.(0))
      else
        match Geneweb.Util.find_sosa_ref conf base with
        | Some pz ->
            let ipz = Gwdb.get_iper pz in
            (* Si on supprime le sosa ... *)
            if ip = ipz then
              match Geneweb.Util.default_sosa_ref conf base with
              | Some p -> Gwdb.get_iper p
              | None -> Gwdb.dummy_iper
            else ipz
        | None -> Gwdb.dummy_iper

let print_del_ind_ok conf base =
  let params = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_index_person in
  let ip = Gwdb.iper_of_string @@ Int32.to_string params.Api_saisie_write_piqi.Index_person.index in
  let p = Gwdb.poi base ip in
  let wl, ml, hr = Geneweb.UpdateIndOk.effective_del conf base p ; [], [], [] in (* FIXME *)
  let ip_redirect = compute_redirect_person conf base p in
  let resp = Api_update_util.UpdateSuccess (wl, ml, hr, None) in
  let data = compute_modification_status conf base ip_redirect Gwdb.dummy_ifam resp in
  Api_util.print_result conf data

(**/**) (* Fonctions de modification famille. *)

let print_del_fam_ok conf base =
  let params = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_index_person_and_family in
  let ip = Gwdb.iper_of_string @@ Int32.to_string params.Api_saisie_write_piqi.Index_person_and_family.index_person in
  let ifam = Gwdb.ifam_of_string @@ Int32.to_string params.Api_saisie_write_piqi.Index_person_and_family.index_family in
  let fam = Gwdb.foi base ifam in
  let wl, ml, hr = Geneweb.UpdateFamOk.effective_del conf base ip fam ; [], [], [] in (* FIXME *)
  let resp = Api_update_util.UpdateSuccess (wl, ml, hr, None) in
  let data = compute_modification_status conf base ip Gwdb.dummy_ifam resp in
  Api_util.print_result conf data

let set_parents_fields conf base p linked created =
  linked.Api_saisie_write_piqi.Person.create_link <- `link;
  created.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
  created.Api_saisie_write_piqi.Person.access <- `access_iftitles;
  created.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
  created.Api_saisie_write_piqi.Person.digest <- "";
  let death_status = infer_death conf base p in
  created.Api_saisie_write_piqi.Person.death_type <- death_status;
  if death_status = `of_course_dead then
    created.Api_saisie_write_piqi.Person.pevents <- created.Api_saisie_write_piqi.Person.pevents @ [ empty_death_pevent () ]

(** [Description] : Permet la factorisation du code pour ajouter une famille
                    et ajouter un enfant à une nouvelle famille.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - p    : la personne à qui on ajoute la famille
    [Retour] :
      - Family : la famille piqi
                                                                           *)
let compute_add_family
      (conf : Geneweb.Config.config) (base : Gwdb.base) (p : Gwdb.person) :
      Api_saisie_write_piqi.family  =
  let adding_to_father = Gwdb.get_sex p = Def.Male in
  let family =
    Api_update_util.piqi_empty_family conf base Gwdb.dummy_ifam
  in
  let p_father =
    if adding_to_father then p else Gwdb.empty_person base Gwdb.dummy_iper
  in
  let p_mother =
    if adding_to_father then Gwdb.empty_person base Gwdb.dummy_iper
    else p
  in
  let father = Api_update_util.pers_to_piqi_mod_person conf base p_father in
  let mother = Api_update_util.pers_to_piqi_mod_person conf base p_mother in
  (* Les index négatifs ne marchent pas ! *)
  (* Par défaut, les access sont en Private, on passe en Iftitles. *)
  family.Api_saisie_write_piqi.Family.index <- Int32.of_string @@ Gwdb.string_of_ifam Gwdb.dummy_ifam;
  if adding_to_father
  then begin
    mother.Api_saisie_write_piqi.Person.sex <- `female ;
    set_parents_fields conf base p father mother
  end
  else begin
    father.Api_saisie_write_piqi.Person.sex <- `male;
    set_parents_fields conf base p mother father
  end ;
  family.Api_saisie_write_piqi.Family.father <- father;
  family.Api_saisie_write_piqi.Family.mother <- mother;
  family

let print_add_family conf base =
  let params = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_index_person in
  let ip = Gwdb.iper_of_string @@ Int32.to_string params.Api_saisie_write_piqi.Index_person.index in
  let p = Gwdb.poi base ip in
  let surname = Gwdb.sou base (Gwdb.get_surname p) in
  let first_name = Gwdb.sou base (Gwdb.get_first_name p) in
  let family = compute_add_family conf base p in
  let add_family =
    {
      Api_saisie_write_piqi.Add_family.person_lastname = surname;
      person_firstname = first_name;
      family = family;
    }
  in
  let data = Api_saisie_write_piqi_ext.gen_add_family add_family in
  Api_util.print_result conf data


(** [Description] : Permet la factorisation du code pour ajouter une famille
                    et ajouter un enfant à une nouvelle famille.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - mod_family : la famille que l'on veut ajouter
    [Retour] :
      - UpdateStatus
                                                                           *)
let compute_add_family_ok'
      (conf : Geneweb.Config.config)
      (base : Gwdb.base)
      (mod_family : Api_saisie_write_piqi.family) :
      (Gwdb.ifam option * Api_update_util.update_base_status) =
  let mod_father = mod_family.Api_saisie_write_piqi.Family.father in
  let mod_mother = mod_family.Api_saisie_write_piqi.Family.mother in
  let moth_fn = mod_mother.Api_saisie_write_piqi.Person.firstname in
  let moth_sn = mod_mother.Api_saisie_write_piqi.Person.lastname in
  mod_mother.Api_saisie_write_piqi.Person.firstname <- moth_fn;
  mod_mother.Api_saisie_write_piqi.Person.lastname <- moth_sn;
  (*
     On ajoute une famille, il faut effectuer les actions suivantes :
       - modification de la personne sur laquelle on clic (pour les clés) => MOD_IND
       - ajout de la famille => ADD_FAM
       - modification de la personne restante => MOD_IND
  *)
  try
    begin
      match (mod_father.Api_saisie_write_piqi.Person.create_link,
             mod_mother.Api_saisie_write_piqi.Person.create_link)
      with
      | (`link, (`create | `create_default_occ)) ->
        let (all_wl, all_ml, all_hr, _cp) =
          match Api_update_person.print_mod conf base mod_father with
          | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (wl, ml, hr, cp)
          | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
          | Api_update_util.UpdateErrorConflict c ->
            raise (Api_update_util.ModErrApiConflict c)
        in
        let ifam_opt, (all_wl, all_ml, all_hr, _cp) =
          match Api_update_family.print_add
                  conf base mod_family mod_father mod_mother
          with
          | ifam_opt, Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> ifam_opt, (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
          | _, Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
          | _, Api_update_util.UpdateErrorConflict c ->
            raise (Api_update_util.ModErrApiConflict c)
        in
        (* Dans le cas d'ajout d'un enfant avec nouveau conjoint, *)
        (* le parent créé vaut ??, donc on ne pourra JAMAIS lui   *)
        (* apporter de modifications.                             *)
        let (all_wl, all_ml, all_hr, cp) =
          if mod_mother.Api_saisie_write_piqi.Person.lastname = "" ||
             mod_mother.Api_saisie_write_piqi.Person.firstname = ""
          then
            (all_wl, all_ml, all_hr, None)
          else
            match
              Api_update_person.print_mod
                conf base {mod_mother with create_link = `create_default_occ}
            with
            | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
            | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
            | Api_update_util.UpdateErrorConflict c ->
              (* On dit que c'est le formulaire de la femme. *)
              c.Api_saisie_write_piqi.Create_conflict.form <- Some `person_form2;
              raise (Api_update_util.ModErrApiConflict c)
        in
        ifam_opt, Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr, cp)
      | ((`create | `create_default_occ), `link) ->
        let (all_wl, all_ml, all_hr, _cp) =
          match Api_update_person.print_mod conf base mod_mother with
          | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (wl, ml, hr, cp)
          | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
          | Api_update_util.UpdateErrorConflict c ->
            (* On dit que c'est le formulaire de la femme. *)
            c.Api_saisie_write_piqi.Create_conflict.form <- Some `person_form2;
            raise (Api_update_util.ModErrApiConflict c)
        in
        let ifam_opt, (all_wl, all_ml, all_hr, cp) =
          match
            Api_update_family.print_add
              conf base mod_family mod_father mod_mother
          with
          | ifam_opt, Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> ifam_opt, (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
          | _, Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
          | _, Api_update_util.UpdateErrorConflict c ->
            raise (Api_update_util.ModErrApiConflict c)
        in
        (* Dans le cas d'ajout d'un enfant avec nouveau conjoint, *)
        (* le parent créé vaut ??, donc on ne pourra JAMAIS lui   *)
        (* apporter de modifications.                             *)
        let (all_wl, all_ml, all_hr, cp) =
          if mod_father.Api_saisie_write_piqi.Person.lastname = ""
          || mod_father.Api_saisie_write_piqi.Person.firstname = ""
          then
            (all_wl, all_ml, all_hr, cp)
          else
            match
              Api_update_person.print_mod
                conf base {mod_father with create_link = `create_default_occ}
            with
            | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
            | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
            | Api_update_util.UpdateErrorConflict c ->
              raise (Api_update_util.ModErrApiConflict c)
        in
        ifam_opt, Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr, cp)
      | (`link, `link) ->
        let (all_wl, all_ml, all_hr, _cp) =
          match Api_update_person.print_mod conf base mod_father with
          | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (wl, ml, hr, cp)
          | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
          | Api_update_util.UpdateErrorConflict c ->
            raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr, _cp) =
          match Api_update_person.print_mod conf base mod_mother with
          | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
          | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
          | Api_update_util.UpdateErrorConflict c ->
            (* On dit que c'est le formulaire de la femme. *)
            c.Api_saisie_write_piqi.Create_conflict.form <- Some `person_form2;
            raise (Api_update_util.ModErrApiConflict c)
        in
        let ifam_opt, (all_wl, all_ml, all_hr, cp) =
          match Api_update_family.print_add
                  conf base mod_family mod_father mod_mother
          with
          | ifam_opt, Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> ifam_opt, (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
          | _, Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
          | _, Api_update_util.UpdateErrorConflict c ->
            raise (Api_update_util.ModErrApiConflict c)
        in
        ifam_opt, Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr, cp)
      | ((`create | `create_default_occ), (`create | `create_default_occ)) ->

        let ifam_opt, (all_wl, all_ml, all_hr, _cp) =
          match Api_update_family.print_add
                  conf base mod_family mod_father mod_mother
          with
          | ifam_opt, Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> ifam_opt, (wl, ml, hr, cp)
          | _, Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
          | _, Api_update_util.UpdateErrorConflict c ->
            raise (Api_update_util.ModErrApiConflict c)
        in

        if mod_father.Api_saisie_write_piqi.Person.lastname = "" then mod_father.Api_saisie_write_piqi.Person.lastname <- "?";
        if mod_father.Api_saisie_write_piqi.Person.firstname = "" then mod_father.Api_saisie_write_piqi.Person.firstname <- "?";
        let (all_wl, all_ml, all_hr, _cp) =
          match
            Api_update_person.print_mod
              conf base {mod_father with create_link = `create_default_occ}
          with
          | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
          | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
          | Api_update_util.UpdateErrorConflict c ->
            raise (Api_update_util.ModErrApiConflict c)
        in


        if mod_mother.Api_saisie_write_piqi.Person.lastname = "" then mod_mother.Api_saisie_write_piqi.Person.lastname <- "?";
        if mod_mother.Api_saisie_write_piqi.Person.firstname = "" then mod_mother.Api_saisie_write_piqi.Person.firstname <- "?";
        let (all_wl, all_ml, all_hr, cp) =
          match
            Api_update_person.print_mod
              conf base {mod_mother with create_link = `create_default_occ}
          with
          | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
          | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
          | Api_update_util.UpdateErrorConflict c ->
            (* On dit que c'est le formulaire de la femme. *)
            c.Api_saisie_write_piqi.Create_conflict.form <- Some `person_form2;
            raise (Api_update_util.ModErrApiConflict c)
        in

        ifam_opt, Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr, cp)
    end
  with
  | Geneweb.Update.ModErr s -> None, Api_update_util.UpdateError s
  | Api_update_util.ModErrApiConflict c -> None, Api_update_util.UpdateErrorConflict c

let compute_add_family_ok conf base family =
  snd @@ compute_add_family_ok' conf base family

let print_add_family_ok conf base =
  let add_family_ok = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_add_family_ok in
  let ip = Gwdb.iper_of_string @@ Int32.to_string add_family_ok.Api_saisie_write_piqi.Add_family_ok.index_person in
  let mod_family = add_family_ok.Api_saisie_write_piqi.Add_family_ok.family in

  let father = mod_family.Api_saisie_write_piqi.Family.father in
  let _fath_occ = father.Api_saisie_write_piqi.Person.occ in

  let mother = mod_family.Api_saisie_write_piqi.Family.mother in
  let _moth_occ = mother.Api_saisie_write_piqi.Person.occ in

  let ifam_opt, resp = compute_add_family_ok' conf base mod_family in
  let ifam = Option.value ifam_opt
      ~default:(Gwdb.ifam_of_string @@ Int32.to_string mod_family.Api_saisie_write_piqi.Family.index)
  in


  let is_dummy = Gwdb.eq_iper Gwdb.dummy_iper in
  let get_infos iper =
    if is_dummy iper then None else
      let p = Gwdb.poi base iper in
      let fn = Gwdb.get_first_name p in
      let sn = Gwdb.get_surname p in
      Some (fn, sn, iper)
  in

  let infos =
    if is_dummy ip then
      let fam = Gwdb.foi base ifam in
      match get_infos (Gwdb.get_father fam) with
      | Some _ as infos -> infos
      | None -> get_infos (Gwdb.get_mother fam)
    else None
  in

  let fn, sn, ip' = match infos with
    | Some (fn, sn, iper) -> fn, sn, iper
    | None ->  Gwdb.empty_string, Gwdb.empty_string, ip
  in

  let response = compute_modification_status' conf base ip ifam resp in

  let index_person = Some (Int32.of_string @@ Gwdb.string_of_iper ip') in

  let firstname =
    if Gwdb.is_empty_string fn then
      response.Api_saisie_write_piqi.Modification_status.firstname
    else Gwdb.sou base fn
  in
  let lastname =
    if Gwdb.is_empty_string sn then
      response.Api_saisie_write_piqi.Modification_status.lastname
    else Gwdb.sou base sn
  in
  let firstname_str =
    if Gwdb.is_empty_string fn then
      response.Api_saisie_write_piqi.Modification_status.firstname_str
    else Some firstname
  in
  let lastname_str =
    if Gwdb.is_empty_string sn then
      response.Api_saisie_write_piqi.Modification_status.lastname_str
    else Some lastname
  in

  let response =
    { response with Api_saisie_write_piqi.Modification_status.index_person;
                    Api_saisie_write_piqi.Modification_status.lastname;
                    Api_saisie_write_piqi.Modification_status.firstname;
                    Api_saisie_write_piqi.Modification_status.lastname_str;
                    Api_saisie_write_piqi.Modification_status.firstname_str;
    }
  in

  let data = Api_saisie_write_piqi_ext.gen_modification_status response in
  Api_util.print_result conf data


let print_mod_family_request conf base =
  let params = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_add_child_request in
  let ip = Gwdb.iper_of_string @@ Int32.to_string params.Api_saisie_write_piqi.Add_child_request.index in
  let p = Gwdb.poi base ip in
  let spouses =
    Array.fold_right
      (fun ifam accu ->
         let cpl = Gwdb.foi base ifam in
         let isp = Gutil.spouse ip cpl in
         let sp = Gwdb.poi base isp in
         let index_family = Int32.of_string @@ Gwdb.string_of_ifam ifam in
         let index_person = Int32.of_string @@ Gwdb.string_of_iper isp in
         let sex =
           match Gwdb.get_sex sp with
           | Def.Male -> `male
           | Def.Female -> `female
           | Def.Neuter -> `unknown
         in
         let lastname = Gwdb.sou base (Gwdb.get_surname sp) in
         let firstname = Gwdb.sou base (Gwdb.get_first_name sp) in
         let dates = Api_util.opt_of_string @@ Api_saisie_read.short_dates_text conf base sp in
         let image = Api_util.get_portrait conf base sp in
         let sosa =
           let sosa_nb = Geneweb.SosaCache.get_single_sosa conf base sp in
           if Sosa.eq sosa_nb Sosa.zero then `no_sosa
           else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
           else `sosa
         in
         let family_spouse =
           {
             Api_saisie_write_piqi.Family_spouse.index_family;
             index_person;
             sex;
             lastname;
             firstname;
             dates ;
             image ;
             sosa ;
           }
         in
         family_spouse :: accu)
      (Gwdb.get_family p) []
  in
  let first_family =
    match Gwdb.get_family p with
    | [||] -> None
    | families ->
      let ifam = Array.get families 0 in
      let fam = Gwdb.foi base ifam in
      let person_lastname = Gwdb.sou base (Gwdb.get_surname p) in
      let person_firstname = Gwdb.sou base (Gwdb.get_first_name p) in
      let family = Api_update_util.fam_to_piqi_mod_family conf base ifam fam in
      let (p_father, p_mother) =
        if Gwdb.get_sex p = Def.Male then (p, Gwdb.poi base (Gutil.spouse ip fam))
        else (Gwdb.poi base (Gutil.spouse ip fam), p)
      in
      let father = Api_update_util.pers_to_piqi_mod_person conf base p_father in
      let mother = Api_update_util.pers_to_piqi_mod_person conf base p_mother in
      (* Mise à jour des parents dans la famille. *)
      family.Api_saisie_write_piqi.Family.father <- father ;
      family.Api_saisie_write_piqi.Family.mother <- mother ;
      Some { Api_saisie_write_piqi.Edit_family.person_lastname ; person_firstname ; family }
  in
  Api_util.print_result conf
    (Api_saisie_write_piqi_ext.gen_edit_family_request
       { Api_saisie_write_piqi.Edit_family_request.spouses ; first_family })


let print_mod_family conf base =
  let params = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_index_person_and_family in
  let ip = Gwdb.iper_of_string @@ Int32.to_string params.Api_saisie_write_piqi.Index_person_and_family.index_person in
  let ifam = Gwdb.ifam_of_string @@ Int32.to_string params.Api_saisie_write_piqi.Index_person_and_family.index_family in
  let p = Gwdb.poi base ip in
  let fam = Gwdb.foi base ifam in
  let surname = Gwdb.sou base (Gwdb.get_surname p) in
  let first_name = Gwdb.sou base (Gwdb.get_first_name p) in
  let family = Api_update_util.fam_to_piqi_mod_family conf base ifam fam in
  let (p_father, p_mother) =
    if Gwdb.get_sex p = Def.Male then (p, Gwdb.poi base (Gutil.spouse ip fam))
    else (Gwdb.poi base (Gutil.spouse ip fam), p)
  in
  let father = Api_update_util.pers_to_piqi_mod_person conf base p_father in
  let mother = Api_update_util.pers_to_piqi_mod_person conf base p_mother in
  (* Mise à jour des parents dans la famille. *)
  let () =
    family.Api_saisie_write_piqi.Family.father <- father;
    family.Api_saisie_write_piqi.Family.mother <- mother;
  in
  let edit_family =
    {
      Api_saisie_write_piqi.Edit_family.person_lastname = surname;
      person_firstname = first_name;
      family = family;
    }
  in
  let data = Api_saisie_write_piqi_ext.gen_edit_family edit_family in
  Api_util.print_result conf data


let print_mod_family_ok conf base =
  let edit_family_ok = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_edit_family_ok in
  let ip = Gwdb.iper_of_string @@ Int32.to_string edit_family_ok.Api_saisie_write_piqi.Edit_family_ok.index_person in
  let mod_family = edit_family_ok.Api_saisie_write_piqi.Edit_family_ok.family in
  let mod_father = mod_family.Api_saisie_write_piqi.Family.father in
  let mod_mother = mod_family.Api_saisie_write_piqi.Family.mother in
  let ifam = Gwdb.ifam_of_string @@ Int32.to_string mod_family.Api_saisie_write_piqi.Family.index in
  let fexclude = [ ifam ] in
  (*
     On modifie une famille, il faut effectuer les actions suivantes :
       - modification du père => MOD_IND
       - modification de la mère => MOD_IND
       - modification de la famille => MOD_FAM
  *)
  let resp =
    try
      begin
        let (all_wl, all_ml, all_hr, cp) =
          if mod_father.Api_saisie_write_piqi.Person.lastname = "?" &&
             mod_father.Api_saisie_write_piqi.Person.firstname = "?"
          then
          (* TODO
            raise (Update.ModErr "PersonKey")
          *)
            ([], [], [], None)
          else
            match Api_update_person.print_mod ~fexclude conf base mod_father with
            | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (wl, ml, hr, cp)
            | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
            | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c)
        in
        mod_family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.create_link <- `link ;
        let (all_wl, all_ml, all_hr, _cp) =
          if mod_mother.Api_saisie_write_piqi.Person.lastname = "?" &&
             mod_mother.Api_saisie_write_piqi.Person.firstname = "?"
          then
          (* TODO
            raise (Update.ModErr "PersonKey")
          *)
            (all_wl, all_ml, all_hr, cp)
          else
            match Api_update_person.print_mod ~fexclude conf base mod_mother with
            | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
            | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
            | Api_update_util.UpdateErrorConflict c ->
                (* On dit que c'est le formulaire de la femme. *)
                c.Api_saisie_write_piqi.Create_conflict.form <- Some `person_form2;
                raise (Api_update_util.ModErrApiConflict c)
        in
        mod_family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.create_link <- `link ;
        let (all_wl, all_ml, all_hr, cp) =
          match Api_update_family.print_mod conf base ip mod_family with
          | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
          | Api_update_util.UpdateError s ->
              raise (Geneweb.Update.ModErr s)
          | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c)
        in
        Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr, cp)
      end
    with
    | Geneweb.Update.ModErr s -> Api_update_util.UpdateError s
    | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c
  in
  let data = compute_modification_status conf base ip ifam resp in
  Api_util.print_result conf data


let print_add_parents conf base =
  let params = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_index_person in
  let ip = Gwdb.iper_of_string @@ Int32.to_string params.Api_saisie_write_piqi.Index_person.index in
  let p = Gwdb.poi base ip in
  let surname = Gwdb.sou base (Gwdb.get_surname p) in
  let first_name = Gwdb.sou base (Gwdb.get_first_name p) in
  let family =
    Api_update_util.piqi_empty_family conf base Gwdb.dummy_ifam
  in
  let father = family.Api_saisie_write_piqi.Family.father in
  let mother = family.Api_saisie_write_piqi.Family.mother in
  (* On supprime le digest car on créé un enfant *)
  father.Api_saisie_write_piqi.Person.digest <- "";
  mother.Api_saisie_write_piqi.Person.digest <- "";
  (* Les index négatifs ne marchent pas ! *)
  family.Api_saisie_write_piqi.Family.index <- Int32.of_string @@ Gwdb.string_of_ifam Gwdb.dummy_ifam;
  father.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
  mother.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
  (* On met à jour la famille avec l'enfant. *)
  let child = Api_update_util.pers_to_piqi_person_link conf base p in
  family.Api_saisie_write_piqi.Family.children <- [child];
  (* On met les parents en mode Create. *)
  father.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
  mother.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
  (* On met à jour les sexes. *)
  father.Api_saisie_write_piqi.Person.sex <- `male;
  mother.Api_saisie_write_piqi.Person.sex <- `female;
  (* On donne aux parents le death_type infere sur p
     et ajoute les évènements nécessaires. *)
  let () =
    let death_status = infer_death conf base p in
    father.Api_saisie_write_piqi.Person.death_type <- death_status;
    mother.Api_saisie_write_piqi.Person.death_type <- death_status;
    if death_status = `of_course_dead then begin
      father.Api_saisie_write_piqi.Person.pevents <- father.Api_saisie_write_piqi.Person.pevents @ [ empty_death_pevent () ];
      mother.Api_saisie_write_piqi.Person.pevents <- mother.Api_saisie_write_piqi.Person.pevents @ [ empty_death_pevent () ]
    end
  in
  father.Api_saisie_write_piqi.Person.lastname <- surname ;
  let add_parents =
    {
      Api_saisie_write_piqi.Add_parents.person_lastname = surname;
      person_firstname = first_name;
      family = family;
    }
  in
  let data = Api_saisie_write_piqi_ext.gen_add_parents add_parents in
  Api_util.print_result conf data

let do_mod_fam_add_child_aux conf base name ip mod_c mod_f fn =
  let resp =
    try
      let child =
        { Api_saisie_write_piqi.Person_link.create_link = mod_c.Api_saisie_write_piqi.Person.create_link
        ; index = mod_c.Api_saisie_write_piqi.Person.index
        ; sex = mod_c.Api_saisie_write_piqi.Person.sex
        ; lastname = mod_c.Api_saisie_write_piqi.Person.lastname
        ; firstname = mod_c.Api_saisie_write_piqi.Person.firstname
        ; occ = mod_c.Api_saisie_write_piqi.Person.occ
        ; dates = None
        }
      in
      mod_f.Api_saisie_write_piqi.Family.children <- mod_f.Api_saisie_write_piqi.Family.children @ [ child ] ;
      let (all_wl, all_ml, all_hr, cp) =
        match fn mod_f with
        | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (wl, ml, hr, cp)
        | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
        | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c)
      in
      if (mod_c.Api_saisie_write_piqi.Person.firstname = "?" || mod_c.Api_saisie_write_piqi.Person.firstname = "")
      && (mod_c.Api_saisie_write_piqi.Person.lastname = "?" || mod_c.Api_saisie_write_piqi.Person.lastname = "")
      then
        Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr, cp)
      else
        let (all_wl, all_ml, all_hr, _cp) =
          let occ = Option.fold ~none:0 ~some:Int32.to_int child.Api_saisie_write_piqi.Person_link.occ in
          match Gwdb.person_of_key base mod_c.Api_saisie_write_piqi.Person.firstname mod_c.Api_saisie_write_piqi.Person.lastname occ with
          | Some ip_child ->
            mod_c.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper ip_child;
            mod_c.Api_saisie_write_piqi.Person.occ <- child.Api_saisie_write_piqi.Person_link.occ;
            let original_create_link = mod_c.create_link in
            mod_c.Api_saisie_write_piqi.Person.create_link <- `link ; (* child has been created already *)
            let digest = Geneweb.Update.digest_person (Geneweb.UpdateInd.string_person_of base @@ Gwdb.poi base ip_child) in
            mod_c.Api_saisie_write_piqi.Person.digest <- digest;
            if mod_c.Api_saisie_write_piqi.Person.death_type = `dont_know_if_dead
            then begin
              let ifam = Gwdb.ifam_of_string @@ Int32.to_string mod_f.Api_saisie_write_piqi.Family.index in
              mod_c.Api_saisie_write_piqi.Person.death_type <-
                Api_util.piqi_death_type_of_death (Geneweb.Update.infer_death_from_parents conf base @@ Gwdb.foi base ifam)
            end ;
            let child_is_created = original_create_link <> `link in
            begin match Api_update_person.print_mod ~with_history:(not child_is_created) conf base mod_c with
              | Api_update_util.UpdateSuccess (wl, ml, hr, cp) ->
                if child_is_created then
                  let child_p = Gwdb.gen_person_of_person (Gwdb.poi base ip_child) in
                  let changed = Def.U_Add_person (Geneweb.Util.string_gen_person base child_p) in
                  let action = "ap" in
                  let child_hr = fun () ->
                    Geneweb.History.record conf base changed action
                  in
                  (all_wl @ wl, all_ml @ ml, child_hr :: all_hr @  hr, cp)
                else
                  (all_wl @ wl, all_ml @ ml, all_hr @  hr, cp)
              | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
              | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c)
            end
          | None -> failwith name
        in
        let occ = Option.fold ~none:Int32.zero ~some:Fun.id child.Api_saisie_write_piqi.Person_link.occ in
        let cp = Some (Api_update_util.created_person ~n:child.lastname ~p:child.firstname ~oc:occ) in
        Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr, cp)
    with
    | Geneweb.Update.ModErr s -> Api_update_util.UpdateError s
    | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c
  in
  let ifam = Gwdb.ifam_of_string @@ Int32.to_string mod_f.Api_saisie_write_piqi.Family.index in
  let data = compute_modification_status conf base ip ifam resp in
  Api_util.print_result conf data

let do_mod_fam_add_child conf base ifam ip mod_c =
  let fam = Gwdb.foi base ifam in
  let mod_f =
    Api_update_util.fam_to_piqi_mod_family conf base ifam fam
  in
  do_mod_fam_add_child_aux
    conf base "do_mod_fam_add_child" ip mod_c mod_f
   (Api_update_family.print_mod conf base ip)

let print_add_child_ok_aux conf base add_child_ok =
  let ip = Gwdb.iper_of_string @@ Int32.to_string add_child_ok.Api_saisie_write_piqi.Add_child_ok.index_person in
  let mod_c = add_child_ok.Api_saisie_write_piqi.Add_child_ok.child in
  if add_child_ok.Api_saisie_write_piqi.Add_child_ok.new_family then begin
    let p = Gwdb.poi base ip in
    let mod_f = compute_add_family conf base p in
    do_mod_fam_add_child_aux
      conf base "ErrorAddChildAndFamily" ip mod_c mod_f
      (compute_add_family_ok conf base)
  end else begin
    let ifam = Gwdb.ifam_of_string @@ Int32.to_string add_child_ok.Api_saisie_write_piqi.Add_child_ok.index_family in
    do_mod_fam_add_child conf base ifam ip mod_c
  end

let print_add_child_ok conf base =
  let add_child_ok = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_add_child_ok in
  print_add_child_ok_aux conf base add_child_ok

let print_add_parents_ok conf base =
  let add_parents_ok = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_add_parents_ok in
  let ip = Gwdb.iper_of_string @@ Int32.to_string add_parents_ok.Api_saisie_write_piqi.Add_parents_ok.index_person in
  let mod_family = add_parents_ok.Api_saisie_write_piqi.Add_parents_ok.family in
  let mod_father = mod_family.Api_saisie_write_piqi.Family.father in
  let mod_mother = mod_family.Api_saisie_write_piqi.Family.mother in
  let existing_fam =
    if mod_father.Api_saisie_write_piqi.Person.create_link = `link
    && mod_mother.Api_saisie_write_piqi.Person.create_link = `link
    then
      let ifath = Gwdb.iper_of_string @@ Int32.to_string mod_father.Api_saisie_write_piqi.Person.index in
      let imoth = Gwdb.iper_of_string @@ Int32.to_string mod_mother.Api_saisie_write_piqi.Person.index in
      let families = Gwdb.get_family (Gwdb.poi base ifath) in
      let len = Array.length families in
      try
        (* Should test compatibility of events and set a warning flag if PossibleDuplicateFam *)
        let ifam =
          let rec loop i =
            if i = len then raise Not_found
            else
              let fam = Gwdb.foi base families.(i) in
              if (Gwdb.get_father fam = ifath && Gwdb.get_mother fam = imoth)
              then families.(i)
              else loop (i + 1)
          in
          loop 0
        in
        Some ifam
      with Not_found -> None
    else None
  in
  (* If both parents are linked, and no extra information is provided,
     we update an existing union (if exists) instead of creating a new one
     (aka add a child instead of add parents) *)
  match existing_fam with
  | Some ifam
    when begin match mod_family.Api_saisie_write_piqi.Family.fevents with
      (* FIXME: really test events compatibilty instead of only handling default case *)
        [ { Api_saisie_write_piqi.Fevent.fevent_type = Some `efam_marriage
          ; date
          ; place = None
          ; reason = None
          ; note = None
          ; src = None
          ; witnesses = []
          ; event_perso = None
          } ] ->
        begin match date with
          | None | Some { Api_saisie_write_piqi.Date.dmy = (Some { Api_saisie_write_piqi.Dmy.year = None ; _ } | None) ; text = None } -> true
          | _ -> false
        end
      | _ -> false
    end ->
    let add_child_ok =
      { Api_saisie_write_piqi.Add_child_ok.index_person = add_parents_ok.Api_saisie_write_piqi.Add_parents_ok.index_person
      ; index_family = Int32.of_string @@ Gwdb.string_of_ifam ifam
      ; new_family = false
      ; child = Api_update_util.pers_to_piqi_mod_person conf base @@ Gwdb.poi base ip
      }
    in
    print_add_child_ok_aux conf base add_child_ok
  | _ ->
  (*
     On ajoute les parents, il faut effectuer les actions suivantes :
       - ajout de la famille => ADD_FAM
       - modification du père => MOD_IND
       - modification de la mère => MOD_IND
  *)
    let resp =
      try
        let (all_wl, all_ml, all_hr, _cp) =
          match snd @@ Api_update_family.print_add conf base mod_family mod_father mod_mother with
          | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (wl, ml, hr, cp)
          | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
          | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c)
        in
        (* Mise à jour des index et digest => fait dans Api_update_family.print_add *)
        let aux p form (all_wl, all_ml, all_hr) =
          let create_link =
            match p.Api_saisie_write_piqi.Person.create_link with
            | `create -> `create_default_occ
            | `create_default_occ | `link as create_link -> create_link
          in
          if p.Api_saisie_write_piqi.Person.firstname = "" then p.Api_saisie_write_piqi.Person.firstname <- "?" ;
          if p.Api_saisie_write_piqi.Person.lastname = "" then p.Api_saisie_write_piqi.Person.lastname <- "?" ;
            match Api_update_person.print_mod ~no_check_name:true conf base {p with create_link} with
            | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
            | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
            | Api_update_util.UpdateErrorConflict c ->
              c.Api_saisie_write_piqi.Create_conflict.form <- Some form ;
              raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr, cp_fath) = aux mod_father `person_form1 (all_wl, all_ml, all_hr) in
        let (all_wl, all_ml, all_hr, cp_moth) = aux mod_mother `person_form2 (all_wl, all_ml, all_hr) in
        let cp = match cp_fath, cp_moth with
          | Some cp, _ when
              mod_father.Api_saisie_write_piqi.Person.create_link <> `link
              && not (Api_update_util.created_person_is_unnamed cp) -> cp_fath
          | _, Some cp when
              mod_mother.Api_saisie_write_piqi.Person.create_link <> `link
            && not (Api_update_util.created_person_is_unnamed cp) -> cp_moth
          | _ -> None
        in
        let all_wl = match existing_fam with
          | Some ifam ->
            let ifam' = Gwdb.ifam_of_string @@ Int32.to_string mod_family.Api_saisie_write_piqi.Family.index in
            Geneweb.Warning.PossibleDuplicateFam (ifam, ifam') :: all_wl
          | _ -> all_wl
        in
        Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr, cp)
      with
      | Geneweb.Update.ModErr s -> Api_update_util.UpdateError s
      | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c
    in
    let data = compute_modification_status conf base ip Gwdb.dummy_ifam resp in
    Api_util.print_result conf data

let i32_of_ifam i = Int32.of_string @@ Gwdb.string_of_ifam i
let i32_of_iper i = Int32.of_string @@ Gwdb.string_of_iper i
let iper_of_i32 i = Gwdb.iper_of_string @@ Int32.to_string i
let ifam_of_i32 i = Gwdb.ifam_of_string @@ Int32.to_string i

let print_add_child conf base =
  let params = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_add_child_request in
  let ip = iper_of_i32 params.Api_saisie_write_piqi.Add_child_request.index in
  let ifam_i32_opt = params.Api_saisie_write_piqi.Add_child_request.index_family in
  let ifam_opt = Option.map ifam_of_i32 ifam_i32_opt in
  let p = Gwdb.poi base ip in
  let families = Gwdb.get_family p in
  let family_spouse =
    Array.fold_right
      (fun ifam accu ->
         let cpl = Gwdb.foi base ifam in
         let isp = Gutil.spouse ip cpl in
         let sp = Gwdb.poi base isp in
         let index_family = i32_of_ifam ifam in
         let index_person = i32_of_iper isp in
         let sex =
           match Gwdb.get_sex sp with
           | Def.Male -> `male
           | Def.Female -> `female
           | Def.Neuter -> `unknown
         in
         let surname = Gwdb.sou base (Gwdb.get_surname sp) in
         let first_name = Gwdb.sou base (Gwdb.get_first_name sp) in
         let dates = Api_saisie_read.short_dates_text conf base sp in
         let image = Api_util.get_portrait conf base sp in
         let sosa =
           let sosa_nb = Geneweb.SosaCache.get_single_sosa conf base sp in
           if Sosa.eq sosa_nb Sosa.zero then `no_sosa
           else if Sosa.eq sosa_nb Sosa.one then `sosa_ref
           else `sosa
         in
         let family_spouse =
           {
             Api_saisie_write_piqi.Family_spouse.index_family = index_family;
             index_person = index_person;
             sex = sex;
             lastname = surname;
             firstname = first_name;
             dates = if dates = "" then None else Some dates;
             image;
             sosa = sosa;
           }
         in
         family_spouse :: accu)
       families []
  in
  let surname = Gwdb.sou base (Gwdb.get_surname p) in
  let first_name = Gwdb.sou base (Gwdb.get_first_name p) in
  let empty_child = Gwdb.empty_person base Gwdb.dummy_iper in
  let child = Api_update_util.pers_to_piqi_mod_person conf base empty_child in
  (* On supprime le digest car on créé un enfant *)
  child.Api_saisie_write_piqi.Person.digest <- "";
  (* Les index négatifs ne marchent pas ! *)
  child.Api_saisie_write_piqi.Person.index <- i32_of_iper Gwdb.dummy_iper;
  (* Par défaut, les access sont en Private, on passe en Iftitles. *)
  child.Api_saisie_write_piqi.Person.access <- `access_iftitles;
  (* On met l'enfant en mode Create. *)
  child.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
  (* On met à jour le sex *)
  let () =
    match params.Api_saisie_write_piqi.Add_child_request.sex with
    | Some sex -> child.Api_saisie_write_piqi.Person.sex <- sex
    | None -> ()
  in
  (* On infere le death status de l'enfant*)
  let () =
    let make_death_status fam =
      let death_status =
        Api_util.piqi_death_type_of_death @@
          Geneweb.Update.infer_death_from_parents conf base (Gwdb.foi base fam) in
      child.Api_saisie_write_piqi.Person.death_type <- death_status;
      if death_status = `of_course_dead then
        child.Api_saisie_write_piqi.Person.pevents <- child.Api_saisie_write_piqi.Person.pevents @ [ empty_death_pevent () ]
    in
    match ifam_opt with
    | None ->
        (* if for some reason ifam is None
           we try with families (wich comes from get_family) *)
        if Array.length families > 0 then
          make_death_status families.(0)
    | Some fam -> make_death_status fam
  in
  (* On prend le nom du père *)
  let child_surname = infer_surname conf base p @@ Option.map Int32.to_string ifam_i32_opt in
  child.Api_saisie_write_piqi.Person.lastname <- child_surname;
  let add_child =
    Api_saisie_write_piqi.Add_child.({
      person_lastname = surname;
      person_firstname = first_name;
      family_spouse = family_spouse;
      child = child;
    })
  in
  let data = Api_saisie_write_piqi_ext.gen_add_child add_child in
  Api_util.print_result conf data

let print_add_sibling conf base =
  let params = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_add_sibling_request in
  let ip = iper_of_i32 params.Api_saisie_write_piqi.Add_sibling_request.index in
  let p = Gwdb.poi base ip in
  let surname = Gwdb.sou base (Gwdb.get_surname p) in
  let first_name = Gwdb.sou base (Gwdb.get_first_name p) in
  let empty_sibling = Gwdb.empty_person base Gwdb.dummy_iper in
  let sibling = Api_update_util.pers_to_piqi_mod_person conf base empty_sibling in
  (* On supprime le digest car on créé un enfant *)
  sibling.Api_saisie_write_piqi.Person.digest <- "";
  (* Les index négatifs ne marchent pas ! *)
  sibling.Api_saisie_write_piqi.Person.index <- i32_of_iper Gwdb.dummy_iper;
  (* Par défaut, les access sont en Private, on passe en Iftitles. *)
  sibling.Api_saisie_write_piqi.Person.access <- `access_iftitles;
  (* On met le frère/soeur en mode Create. *)
  sibling.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
  (* On met à jour le sex *)
  Option.iter (fun s -> sibling.Api_saisie_write_piqi.Person.sex <- s) params.Api_saisie_write_piqi.Add_sibling_request.sex ;
  let fam_opt = Option.map (Gwdb.foi base) (Gwdb.get_parents p) in
  (* On infere un death_type *)
  let () =
    match fam_opt with
    | None -> sibling.Api_saisie_write_piqi.Person.death_type <- `not_dead
    | Some fam ->
      let death_status =
        Api_util.piqi_death_type_of_death @@
          Geneweb.Update.infer_death_from_parents conf base fam in
      sibling.Api_saisie_write_piqi.Person.death_type <- death_status;
      if death_status = `of_course_dead then
        sibling.Api_saisie_write_piqi.Person.pevents <- sibling.Api_saisie_write_piqi.Person.pevents @ [ empty_death_pevent () ]
  in
  (* On prend le nom du père *)
  let sibling_surname =
    match Option.map (fun fam -> Gwdb.poi base @@ Gwdb.get_father fam) fam_opt with
    | Some father -> infer_surname conf base father None
    | None -> surname
  in
  sibling.Api_saisie_write_piqi.Person.lastname <- sibling_surname;
  let add_sibling =
    Api_saisie_write_piqi.Add_sibling.({
      person_lastname = surname;
      person_firstname = first_name;
      sibling = sibling;
    })
  in
  let data = Api_saisie_write_piqi_ext.gen_add_sibling add_sibling in
  Api_util.print_result conf data


let print_add_sibling_ok conf base =
  let add_sibling_ok = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_add_sibling_ok in
  let ip = iper_of_i32 add_sibling_ok.Api_saisie_write_piqi.Add_sibling_ok.index_person in
  let mod_c = add_sibling_ok.Api_saisie_write_piqi.Add_sibling_ok.sibling in
  let p = Gwdb.poi base ip in
  (* Le nouvel enfant à créer. *)
  let fn = mod_c.Api_saisie_write_piqi.Person.firstname in
  let sn = mod_c.Api_saisie_write_piqi.Person.lastname in
  let occ = mod_c.Api_saisie_write_piqi.Person.occ in
  let create_sibling =
    {
      Api_saisie_write_piqi.Person_link.create_link = mod_c.Api_saisie_write_piqi.Person.create_link;
      index = mod_c.Api_saisie_write_piqi.Person.index;
      sex = mod_c.Api_saisie_write_piqi.Person.sex;
      lastname = sn;
      firstname = fn;
      occ = occ; (* Directement mis à jour dans update_family *)
      dates = None;
    }
  in
  (* Si il n'y a pas de parent, on veut créer la famille *)
  match Gwdb.get_parents p with
  | None ->
      begin
        (*
           On ajoute une famille, il faut effectuer les actions suivantes :
             - ajout d'une famille et de l'enfant => ADD_FAM
             - modification de l'enfant => MOD_IND
        *)
        let new_ifam = ref Gwdb.dummy_ifam in
        let resp =
          try
            (* TODO compute add_parents. *)
            let family =
              Api_update_util.piqi_empty_family conf base Gwdb.dummy_ifam
            in
            let father = family.Api_saisie_write_piqi.Family.father in
            let mother = family.Api_saisie_write_piqi.Family.mother in
            (* On supprime le digest car on créé un enfant *)
            father.Api_saisie_write_piqi.Person.digest <- "";
            mother.Api_saisie_write_piqi.Person.digest <- "";
            (* Les index négatifs ne marchent pas ! *)
            family.Api_saisie_write_piqi.Family.index <- Int32.of_string @@ Gwdb.string_of_ifam Gwdb.dummy_ifam;
            father.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
            mother.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
            (* On met à jour la famille avec l'enfant. *)
            let child = Api_update_util.pers_to_piqi_person_link conf base p in
            family.Api_saisie_write_piqi.Family.children <- [child; create_sibling];
            (* On met les parents en mode Create. *)
            father.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
            mother.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
            (* On met à jour les sexes. *)
            father.Api_saisie_write_piqi.Person.sex <- `male;
            mother.Api_saisie_write_piqi.Person.sex <- `female;
            (* On ajoute la famille : ADD_FAM *)
            let (all_wl, all_ml, all_hr, cp) =
              match
                snd @@ Api_update_family.print_add conf base family father mother
              with
              | Api_update_util.UpdateSuccess (wl, ml, hr, cp) ->
                  (* On ajoute une famille donc l'ifam est nouveau *)
                  let () = new_ifam := Gwdb.ifam_of_string @@ Int32.to_string family.Api_saisie_write_piqi.Family.index in
                  (wl, ml, hr, cp)
              | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
              | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c)
            in
            if (fn = "?" || fn = "") &&
               (sn = "?" || sn = "")
            then
              Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr, cp)
            else
              (* On met à jour l'enfant et l'index ! *)
              let (all_wl, all_ml, all_hr, cp) =
                let occ =
                  match create_sibling.Api_saisie_write_piqi.Person_link.occ with
                  | None -> 0
                  | Some occ -> Int32.to_int occ
                in
                match Gwdb.person_of_key base fn sn occ with
                | Some ip_sibling ->
                    mod_c.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper ip_sibling;
                    mod_c.Api_saisie_write_piqi.Person.occ <- create_sibling.Api_saisie_write_piqi.Person_link.occ;
                    (* On calcul le digest maintenant que l'enfant est créé. *)
                    let sibling = Gwdb.poi base ip_sibling in
                    let digest = Geneweb.Update.digest_person (Geneweb.UpdateInd.string_person_of base sibling) in
                    let create_link =
                      match mod_c.Api_saisie_write_piqi.Person.create_link with
                      | `create -> `create_default_occ
                      | `create_default_occ | `link as create_link ->
                         create_link
                    in
                    mod_c.Api_saisie_write_piqi.Person.digest <- digest;
                    (match
                       Api_update_person.print_mod
                         conf base {mod_c with create_link}
                     with
                    | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
                    | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
                    | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c))
                | None -> failwith "ErrorAddSiblingAndFamily"
              in
              Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr, cp)
          with
          | Geneweb.Update.ModErr s -> Api_update_util.UpdateError s
          | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c
        in
        let data = compute_modification_status conf base ip !new_ifam resp in
        Api_util.print_result conf data
      end
  | Some ifam ->
      (*
         On modifie une famille, il faut effectuer les actions suivantes :
           - modification de la famille => MOD_FAM
           - modification de l'enfant => MOD_IND
      *)
      begin
        let fam = Gwdb.foi base ifam in
        let mod_f =
          Api_update_util.fam_to_piqi_mod_family conf base ifam fam
        in
        (* On ajoute le nouvel enfant. *)
        mod_f.Api_saisie_write_piqi.Family.children <-
          mod_f.Api_saisie_write_piqi.Family.children @ [create_sibling];
        let resp =
          try
            begin
              let (all_wl, all_ml, all_hr, cp) =
                match Api_update_family.print_mod conf base ip mod_f with
                | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (wl, ml, hr, cp)
                | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
                | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c)
              in
              if (fn = "?" || fn = "") &&
                 (sn = "?" || sn = "")
              then
                Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr, cp)
              else
                (* On met à jour l'enfant et l'index ! *)
                let (all_wl, all_ml, all_hr, cp) =
                  let occ =
                    match create_sibling.Api_saisie_write_piqi.Person_link.occ with
                    | None -> 0
                    | Some occ -> Int32.to_int occ
                  in
                  match Gwdb.person_of_key base fn sn occ with
                  | Some ip_sibling ->
                      mod_c.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper ip_sibling;
                      mod_c.Api_saisie_write_piqi.Person.occ <- create_sibling.Api_saisie_write_piqi.Person_link.occ;
                      (* On calcul le digest maintenant que l'enfant est créé. *)
                      let sibling = Gwdb.poi base ip_sibling in
                      let digest = Geneweb.Update.digest_person (Geneweb.UpdateInd.string_person_of base sibling) in
                      let create_link =
                        match
                          mod_c.Api_saisie_write_piqi.Person.create_link
                        with
                        | `create -> `create_default_occ
                        | `create_default_occ | `link as create_link ->
                           create_link
                      in
                      mod_c.Api_saisie_write_piqi.Person.digest <- digest;
                      (match
                         Api_update_person.print_mod
                           conf base {mod_c with create_link}
                       with
                       | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
                       | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
                       | Api_update_util.UpdateErrorConflict c -> raise (Api_update_util.ModErrApiConflict c))
                  | None -> failwith "ErrorAddSibling"
                in
                Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr, cp)
            end
          with
          | Geneweb.Update.ModErr s -> Api_update_util.UpdateError s
          | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c
        in
        let data = compute_modification_status conf base ip ifam resp in
        Api_util.print_result conf data
      end


(**/**) (* Fonctions pour la première saisie. *)

let raise_ModErr e = raise (Geneweb.Update.ModErr e)

(** [check_input_person p] checks that every needed field of [p] is filled *)
let check_input_person mod_p : 'unit_or_exn =
  let o =
    match mod_p.Api_saisie_write_piqi.Person.occ with
    | Some i -> Int32.to_int i
    | None -> 0
  in
  let f = mod_p.Api_saisie_write_piqi.Person.firstname in
  let s = mod_p.Api_saisie_write_piqi.Person.lastname in
  if f = "" && s = "" then
    List.fold_right begin fun evt () ->
      match evt.Api_saisie_write_piqi.Pevent.date with
      | None -> ()
      | Some date ->
        match date.Api_saisie_write_piqi.Date.dmy with
        | None -> ()
        | Some dmy ->
          match dmy.Api_saisie_write_piqi.Dmy.year, dmy.Api_saisie_write_piqi.Dmy.month, dmy.Api_saisie_write_piqi.Dmy.day with
          | (None, None, None) -> ()
          | _ -> raise_ModErr (Geneweb.Update.UERR_unknow_person (f, s, o))
    end mod_p.Api_saisie_write_piqi.Person.pevents ()
  else if s = "" then
    let designation = mod_p.Api_saisie_write_piqi.Person.firstname ^ "." ^ string_of_int o ^ " ?" in
    let designation = (Geneweb.Util.escape_html designation : Adef.escaped_string :> Adef.safe_string) in
    raise_ModErr (Geneweb.Update.UERR_missing_surname designation)
  else if f = "" then
    let designation = "?." ^ string_of_int o ^ " " ^ mod_p.Api_saisie_write_piqi.Person.lastname in
    let designation = (Geneweb.Util.escape_html designation : Adef.escaped_string :> Adef.safe_string) in
    raise_ModErr (Geneweb.Update.UERR_missing_first_name designation)
  else if mod_p.Api_saisie_write_piqi.Person.sex = `unknown then
    raise_ModErr (Geneweb.Update.UERR_sex_undefined (f, s, o))

let compute_add_first_fam conf =
  let add_first_fam = Api_util.get_params conf Api_saisie_write_piqi_ext.parse_add_first_fam in

  (* On ré-initialise un certain nombre de valeurs. *)
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.sosa.Api_saisie_write_piqi.Person.digest <- "";
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.sosa.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.sosa.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.sosa.Api_saisie_write_piqi.Person.occ <- None;
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.sosa.Api_saisie_write_piqi.Person.access <- `access_iftitles;

  add_first_fam.Api_saisie_write_piqi.Add_first_fam.father.Api_saisie_write_piqi.Person.digest <- "";
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.father.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.father.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
  (* On n'autorise pas les parents de meme sexe. *)
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.father.Api_saisie_write_piqi.Person.sex <- `male;
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.father.Api_saisie_write_piqi.Person.occ <- None;
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.father.Api_saisie_write_piqi.Person.access <- `access_iftitles;

  add_first_fam.Api_saisie_write_piqi.Add_first_fam.mother.Api_saisie_write_piqi.Person.digest <- "";
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.mother.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.mother.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
  (* On n'autorise pas les parents de meme sexe. *)
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.mother.Api_saisie_write_piqi.Person.sex <- `female;
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.mother.Api_saisie_write_piqi.Person.occ <- None;
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.mother.Api_saisie_write_piqi.Person.access <- `access_iftitles;

  add_first_fam.Api_saisie_write_piqi.Add_first_fam.spouse.Api_saisie_write_piqi.Person.digest <- "";
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.spouse.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.spouse.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.spouse.Api_saisie_write_piqi.Person.occ <- None;
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.spouse.Api_saisie_write_piqi.Person.access <- `access_iftitles;

  (* On strip aussi les enfants. *)
  add_first_fam.Api_saisie_write_piqi.Add_first_fam.children <-
    List.fold_right
      (fun mod_c accu ->
        if mod_c.Api_saisie_write_piqi.Person.lastname = "" &&
           mod_c.Api_saisie_write_piqi.Person.firstname = ""
        then accu
        else
          begin
            mod_c.Api_saisie_write_piqi.Person.digest <- "";
            mod_c.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
            mod_c.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
            mod_c.Api_saisie_write_piqi.Person.occ <- None;
            mod_c.Api_saisie_write_piqi.Person.access <- `access_iftitles;
            mod_c :: accu
          end)
      add_first_fam.Api_saisie_write_piqi.Add_first_fam.children [];

  let mod_p = add_first_fam.Api_saisie_write_piqi.Add_first_fam.sosa in
  let mod_father = add_first_fam.Api_saisie_write_piqi.Add_first_fam.father in
  let mod_mother = add_first_fam.Api_saisie_write_piqi.Add_first_fam.mother in
  let mod_spouse = add_first_fam.Api_saisie_write_piqi.Add_first_fam.spouse in
  let mod_children = add_first_fam.Api_saisie_write_piqi.Add_first_fam.children in

  (* On vérifie toutes les erreurs de saisie possibles. *)
  (* Attention, il faut envoyer dans le même ordre que pour la saisie, *)
  (* sinon les occurrences seront toutes changées !!!                  *)
  let resp =
    try
      begin
        (* On vérifie qu'il n'y a pas de problème de nom/prénom. *)
        check_input_person mod_p;
        check_input_person mod_father;
        check_input_person mod_mother;
        check_input_person mod_spouse;
        List.iter check_input_person mod_children;

        let (all_wl, all_ml, all_hr, _cp) =
          match Api_update_person.print_add_nobase conf mod_father with
          | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (wl, ml, hr, cp)
          | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
          | Api_update_util.UpdateErrorConflict c ->
              raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr, _cp) =
          match Api_update_person.print_add_nobase conf mod_mother with
          | Api_update_util.UpdateSuccess (wl, ml, hr, cp) ->
              (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
          | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
          | Api_update_util.UpdateErrorConflict c ->
              raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr, _cp) =
          match Api_update_person.print_add_nobase conf mod_p with
          | Api_update_util.UpdateSuccess (wl, ml, hr, cp) ->
              (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
          | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
          | Api_update_util.UpdateErrorConflict c ->
              raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr, cp) =
          match Api_update_person.print_add_nobase conf mod_spouse with
          | Api_update_util.UpdateSuccess (wl, ml, hr, cp) ->
              (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
          | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
          | Api_update_util.UpdateErrorConflict c ->
              raise (Api_update_util.ModErrApiConflict c)
        in
        let (all_wl, all_ml, all_hr, cp) =
          List.fold_left
            (fun (all_wl, all_ml, all_hr, _cp) mod_child ->
              match Api_update_person.print_add_nobase conf mod_child with
              | Api_update_util.UpdateSuccess (wl, ml, hr, cp) ->
                  (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
              | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
              | Api_update_util.UpdateErrorConflict c ->
                  raise (Api_update_util.ModErrApiConflict c))
            (all_wl, all_ml, all_hr, cp) mod_children
        in
        Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr, cp)
      end
    with
    | Geneweb.Update.ModErr s -> Api_update_util.UpdateError s
    | Api_update_util.ModErrApiConflict c ->
        Api_update_util.UpdateErrorConflict c
  in
  (add_first_fam, resp)

(** [print_add_first_fam conf]
    Simulate the addition of the first family in an empty base, without
    any existing base.
*)
let print_add_first_fam conf =
  let (add_first_fam, resp) = compute_add_first_fam conf in
  let mod_p = add_first_fam.Api_saisie_write_piqi.Add_first_fam.sosa in
  let lastname = mod_p.Api_saisie_write_piqi.Person.lastname in
  let firstname = mod_p.Api_saisie_write_piqi.Person.firstname in
  let occ = mod_p.Api_saisie_write_piqi.Person.occ in
  let lastname_str = Some mod_p.Api_saisie_write_piqi.Person.lastname in
  let firstname_str = Some mod_p.Api_saisie_write_piqi.Person.firstname in
  let n = if lastname = "" then None else Some (Name.lower lastname) in
  let p = if firstname = "" then None else Some (Name.lower firstname) in
  let index_family = None in
  let (is_base_updated, warnings, miscs, conflict, _) =
    match resp with
    | Api_update_util.UpdateErrorConflict c -> (false, [], [], Some c, [])
    | Api_update_util.UpdateError s -> (false, [s], [], None, [])
    | Api_update_util.UpdateSuccess _ -> (true, [], [], None, [])
  in
  let response =
    let open Api_util in
    { Api_saisie_write_piqi.Modification_status.is_base_updated
    ; base_warnings = List.map (fun s -> !!(Geneweb.Update.string_of_error conf s)) warnings
    ; base_miscs = miscs
    ; index_person = None
    ; lastname
    ; firstname
    ; occ
    ; index_family
    ; conflict
    ; lastname_str
    ; firstname_str
    ; n
    ; p
    ; created_person = None
    }
  in
  let data = Api_saisie_write_piqi_ext.gen_modification_status response in
  Api_util.print_result conf data


let print_add_first_fam_ok conf base =
  let (add_first_fam, _) = compute_add_first_fam conf in
  let mod_p = add_first_fam.Api_saisie_write_piqi.Add_first_fam.sosa in
  let mod_father = add_first_fam.Api_saisie_write_piqi.Add_first_fam.father in
  let mod_mother = add_first_fam.Api_saisie_write_piqi.Add_first_fam.mother in
  let mod_spouse = add_first_fam.Api_saisie_write_piqi.Add_first_fam.spouse in
  let mod_children = add_first_fam.Api_saisie_write_piqi.Add_first_fam.children in

  (* Pour l'instant, on a pas d'ip. *)
  let ip = ref Gwdb.dummy_iper in
  let ifam = ref Gwdb.dummy_ifam in

  let resp =
    try
      begin
        (* On crée la famille avec les parents. *)
        let fam_asc =
          let family =
            Api_update_util.piqi_empty_family conf base Gwdb.dummy_ifam
          in
          (* On ré-initialise un certain nombre de valeurs, *)
          (* surtout si c'est des personnes vides.          *)
          family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.digest <- "";
          family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
          family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
          family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.occ <- None;
          family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.access <- `access_iftitles;

          family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.digest <- "";
          family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
          family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
          family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.occ <- None;
          family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.access <- `access_iftitles;

          (* On remplace les parents. *)
          if mod_father.Api_saisie_write_piqi.Person.lastname = "" &&
             mod_father.Api_saisie_write_piqi.Person.firstname = ""
          then ()
          else family.Api_saisie_write_piqi.Family.father <- mod_father;
          if mod_mother.Api_saisie_write_piqi.Person.lastname = "" &&
             mod_mother.Api_saisie_write_piqi.Person.firstname = ""
          then ()
          else family.Api_saisie_write_piqi.Family.mother <- mod_mother;
          (* Les index négatifs ne marchent pas ! *)
          family.Api_saisie_write_piqi.Family.index <- Int32.of_string @@ Gwdb.string_of_ifam Gwdb.dummy_ifam;
          family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
          family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
          (* On met à jour les sexes. *)
          family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.sex <- `male;
          family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.sex <- `female;
          (* On met à jour la famille avec l'enfant. *)
          let child =
            {
              Api_saisie_write_piqi.Person_link.create_link = `create_default_occ;
              index = mod_p.Api_saisie_write_piqi.Person.index;
              sex = mod_p.Api_saisie_write_piqi.Person.sex;
              lastname = mod_p.Api_saisie_write_piqi.Person.lastname;
              firstname = mod_p.Api_saisie_write_piqi.Person.firstname;
              occ = mod_p.Api_saisie_write_piqi.Person.occ;
              dates = None;
            }
          in
          family.Api_saisie_write_piqi.Family.children <- [child];
          family
        in

        (* Ajout de fam_asc. *)
        let (all_wl, all_ml, all_hr, cp) =
          (* La personne n'a pas de parents. *)
          if (fam_asc.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.firstname = "" &&
              fam_asc.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.lastname = "" &&
              fam_asc.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.firstname = "" &&
              fam_asc.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.lastname = "")
          then
            let (all_wl, all_ml, all_hr, cp) =
              match Api_update_person.print_add conf base mod_p with
              | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (wl, ml, hr, cp)
              | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
              | Api_update_util.UpdateErrorConflict c ->
                  raise (Api_update_util.ModErrApiConflict c)
            in
            (* On met à jour l'index. *)
            let () =
              let (sn, fn) =
                (mod_p.Api_saisie_write_piqi.Person.lastname,
                 mod_p.Api_saisie_write_piqi.Person.firstname)
              in
              let occ =
                match mod_p.Api_saisie_write_piqi.Person.occ with
                | None -> 0
                | Some occ -> Int32.to_int occ
              in
              match Gwdb.person_of_key base fn sn occ with
              | Some ip ->
                  mod_p.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper ip
              | None -> failwith "ErrorAddFirstFamNoChildFound"
            in
            (all_wl, all_ml, all_hr, cp)
          else
            (* On ajoute la famille avec les parents. *)
            let (all_wl, all_ml, all_hr, _cp) =
              match compute_add_family_ok conf base fam_asc with
              | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (wl, ml, hr, cp)
              | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
              | Api_update_util.UpdateErrorConflict c ->
                  raise (Api_update_util.ModErrApiConflict c)
            in
            (* On modifie la personne "principale". *)
            let (all_wl, all_ml, all_hr, cp) =
              match fam_asc.Api_saisie_write_piqi.Family.children with
              | [create_child] ->
                  let (sn, fn) =
                    (mod_p.Api_saisie_write_piqi.Person.lastname,
                     mod_p.Api_saisie_write_piqi.Person.firstname)
                  in
                  let occ =
                    match create_child.Api_saisie_write_piqi.Person_link.occ with
                    | None -> 0
                    | Some occ -> Int32.to_int occ
                  in
                  (match Gwdb.person_of_key base fn sn occ with
                  | Some ip_child ->
                      mod_p.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper ip_child;
                      mod_p.Api_saisie_write_piqi.Person.occ <-
                        create_child.Api_saisie_write_piqi.Person_link.occ;
                      (* On calcul le digest maintenant que l'enfant est créé. *)
                      let child = Gwdb.poi base ip_child in
                      let digest =
                        Geneweb.Update.digest_person (Geneweb.UpdateInd.string_person_of base child)
                      in
                      mod_p.Api_saisie_write_piqi.Person.digest <- digest;
                      (match Api_update_person.print_mod conf base mod_p with
                      | Api_update_util.UpdateSuccess (wl, ml, hr, cp) ->
                          (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
                      | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
                      | Api_update_util.UpdateErrorConflict c ->
                          raise (Api_update_util.ModErrApiConflict c))
                  | None -> failwith "ErrorAddFirstFamNoChildFound")
              | _ -> failwith "ErrorAddFirstFamNoChild"
            in
            (all_wl, all_ml, all_hr, cp)
        in

        (* Normalement, on a réussi à mettre à jour l'ip de la personne. *)
        let () = ip := Gwdb.iper_of_string @@ Int32.to_string mod_p.Api_saisie_write_piqi.Person.index in
        let () = ifam := Gwdb.ifam_of_string @@ Int32.to_string fam_asc.Api_saisie_write_piqi.Family.index in

        (* On crée la famille avec les enfants. *)
        let fam_desc =
          let family =
            Api_update_util.piqi_empty_family conf base Gwdb.dummy_ifam
          in
          (* On ré-initialise un certain nombre de valeurs, *)
          (* surtout si c'est des personnes vides.          *)
          family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.digest <- "";
          family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
          family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
          family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.occ <- None;
          family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.access <- `access_iftitles;

          family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.digest <- "";
          family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
          family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
          family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.occ <- None;
          family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.access <- `access_iftitles;

          (* On remplace les parents. *)
          if mod_p.Api_saisie_write_piqi.Person.sex = `male then
            begin
              family.Api_saisie_write_piqi.Family.father <- mod_p;
              if mod_spouse.Api_saisie_write_piqi.Person.lastname = "" &&
                 mod_spouse.Api_saisie_write_piqi.Person.firstname = ""
              then ()
              else family.Api_saisie_write_piqi.Family.mother <- mod_spouse;
            end
          else
            begin
              if mod_spouse.Api_saisie_write_piqi.Person.lastname = "" &&
                 mod_spouse.Api_saisie_write_piqi.Person.firstname = ""
              then ()
              else family.Api_saisie_write_piqi.Family.father <- mod_spouse;
              family.Api_saisie_write_piqi.Family.mother <- mod_p;
            end;
          (* Les index négatifs ne marchent pas ! *)
          family.Api_saisie_write_piqi.Family.index <- Int32.of_string @@ Gwdb.string_of_ifam Gwdb.dummy_ifam;
          (* On n'autorise pas les parents de meme sexe. *)
          (* On met les parents en mode Create. *)
          if mod_p.Api_saisie_write_piqi.Person.sex = `male then
            begin
              family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.create_link <- `link;
              let p = Gwdb.poi base !ip in
              let digest = Geneweb.Update.digest_person (Geneweb.UpdateInd.string_person_of base p) in
              family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.digest <- digest;
              family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
              family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
            end
          else
            begin
              family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.create_link <- `create_default_occ;
              family.Api_saisie_write_piqi.Family.father.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
              family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.create_link <- `link;
              let p = Gwdb.poi base !ip in
              let digest = Geneweb.Update.digest_person (Geneweb.UpdateInd.string_person_of base p) in
              family.Api_saisie_write_piqi.Family.mother.Api_saisie_write_piqi.Person.digest <- digest;
            end;
          (* On met à jour la famille avec les enfants. *)
          let children =
            List.map
              (fun c ->
                {
                  Api_saisie_write_piqi.Person_link.create_link = `create_default_occ;
                  index = c.Api_saisie_write_piqi.Person.index;
                  sex = c.Api_saisie_write_piqi.Person.sex;
                  lastname = c.Api_saisie_write_piqi.Person.lastname;
                  firstname = c.Api_saisie_write_piqi.Person.firstname;
                  occ = c.Api_saisie_write_piqi.Person.occ;
                  dates = None;
                })
              mod_children
          in
          family.Api_saisie_write_piqi.Family.children <- children;
          family
        in

        (* On ajoute la famille avec les enfants. *)
        (* S'il n'y a pas de descendance, on ne fait pas l'ajout. *)
        let (all_wl, all_ml, all_hr) =
          if (mod_spouse.Api_saisie_write_piqi.Person.firstname = "" &&
              mod_spouse.Api_saisie_write_piqi.Person.lastname = "" &&
              mod_children = [])
          then (all_wl, all_ml, all_hr)
          else
            let (all_wl, all_ml, all_hr, cp) =
              match compute_add_family_ok conf base fam_desc with
              | Api_update_util.UpdateSuccess (wl, ml, hr, cp) -> (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
              | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
              | Api_update_util.UpdateErrorConflict c ->
                  raise (Api_update_util.ModErrApiConflict c)
            in
            let (all_wl, all_ml, all_hr, _cp) =
              List.fold_left
                (fun (all_wl, all_ml, all_hr, _cp) mod_child ->
                  let (sn, fn) =
                    (mod_child.Api_saisie_write_piqi.Person.lastname,
                     mod_child.Api_saisie_write_piqi.Person.firstname)
                  in
                  let occ =
                    match mod_child.Api_saisie_write_piqi.Person.occ with
                    | None -> 0
                    | Some occ -> Int32.to_int occ
                  in
                  (match Gwdb.person_of_key base fn sn occ with
                  | Some ip_child ->
                      mod_child.Api_saisie_write_piqi.Person.index <- Int32.of_string @@ Gwdb.string_of_iper ip_child;
                      (* On calcul le digest maintenant que l'enfant est créé. *)
                      let child = Gwdb.poi base ip_child in
                      let digest =
                        Geneweb.Update.digest_person (Geneweb.UpdateInd.string_person_of base child)
                      in
                      mod_child.Api_saisie_write_piqi.Person.digest <- digest;
                      (match Api_update_person.print_mod conf base mod_child with
                      | Api_update_util.UpdateSuccess (wl, ml, hr, cp) ->
                          (all_wl @ wl, all_ml @ ml, all_hr @ hr, cp)
                      | Api_update_util.UpdateError s -> raise (Geneweb.Update.ModErr s)
                      | Api_update_util.UpdateErrorConflict c ->
                          raise (Api_update_util.ModErrApiConflict c))
                  | None -> failwith "ErrorAddFirstFamNoChildFound"))
                (all_wl, all_ml, all_hr, cp) mod_children
            in
            (all_wl, all_ml, all_hr)
        in

        Api_update_util.UpdateSuccess (all_wl, all_ml, all_hr, cp)
      end
    with
    | Geneweb.Update.ModErr s -> Api_update_util.UpdateError s
    | Api_update_util.ModErrApiConflict c -> Api_update_util.UpdateErrorConflict c
  in
  let data = compute_modification_status conf base !ip !ifam resp in
  Api_util.print_result conf data
