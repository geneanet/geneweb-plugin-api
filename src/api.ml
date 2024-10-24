(**/**) (* Services disponibles. *)

let print_info_base conf base =
  let (sosa_p, sosa) =
     match Geneweb.Util.find_sosa_ref conf base with
     | Some p -> (Some p, Some (Api_util.person_to_reference_person base p))
     | None -> (None, None)
  in
  let last_modified_person =
    let default () = Option.map (fun p -> Gwdb.string_of_iper (Gwdb.get_iper p)) sosa_p in
    try
      let ic = Secure.open_in_bin (Geneweb.History.file_name conf) in
      let (_, pos, wiz) = (1, in_channel_length ic, "") in
      let vv = (ref (Bytes.create 0), ref 0) in
      let last_modified_person =
        let (line, _) = Mutil.rev_input_line ic pos vv in
        match Geneweb.History.line_fields line with
        | Some (_, user, action, keyo) ->
          if wiz = "" || user = wiz then
            match keyo with
            | Some key ->
              (match action with
               | "mn" -> default ()
               | _ ->
                 (match Gutil.person_ht_find_all base key with
                  | [ip] -> Some (Gwdb.string_of_iper ip)
                  | _ -> default ()))
            | None -> default ()
          else default ()
        | None -> default ()
      in
      close_in ic;
      last_modified_person
    with Sys_error _ | _ -> default ()
  in
  let has_ignored_duplicates = Some (Geneweb.GWPARAM.has_ignored_duplicates conf base) in
  let info_base =
    Api_piqi.Infos_base.({
      nb_persons = Int64.of_int (Gwdb.nb_of_persons base);
      nb_families = Int64.of_int (Gwdb.nb_of_families base);
      sosa = sosa;
      last_modified_person = Option.map Int64.of_string last_modified_person;
      real_nb_persons = Some (Int64.of_int (Gwdb.nb_of_real_persons base));
      has_ignored_duplicates;
    })
  in
  let data = Api_piqi_ext.gen_infos_base info_base in
  Api_util.print_result conf data

let print_loop conf base =
  let (base_loop, pers) =
    (ref false, ref (Gwdb.poi base (Gwdb.dummy_iper)))
  in
  (* On ne fait pas un Geneweb.Util.create_topological_sort conf base qui est certe *)
  (* plus rapide, mais qui dans de rare cas, n'est pas capable de remonter  *)
  (* la boucle (on ne check pas la base en entier). Avec cette méthode, on  *)
  (* n'a pas de ce problème.                                                *)
  let () =
    Gwdb.load_ascends_array base ;
    Gwdb.load_couples_array base ;
    Consang.check_noloop base
      (function OwnAncestor p -> base_loop := true ; pers := p | _ -> () ) ;
    Gwdb.clear_ascends_array base ;
    Gwdb.clear_couples_array base
  in
  let p =
    if !base_loop then
      Api_util.pers_to_piqi_person conf base !pers (fun _ -> Sosa.zero)
    else
      let ref_pers = Api_util.empty_reference_person in
      Api_util.empty_piqi_person conf ref_pers
  in
  let data = Api_util.data_person p in
  Api_util.print_result conf data

let print_info_ind conf base =
  let ref_person = Api_util.get_params conf Api_piqi_ext.parse_reference_person in
  let filters = Api_util.get_filters conf in
  let compute_sosa = Api_util.compute_sosa conf base true in
  let sn = ref_person.Api_piqi.Reference_person.n in
  let fn = ref_person.Api_piqi.Reference_person.p in
  let occ = ref_person.Api_piqi.Reference_person.oc in
  let p =
    match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
    | Some ip ->
        let p = Geneweb.Util.pget conf base ip in
        if Api_util.apply_filters_p conf filters compute_sosa p then
          Api_util.pers_to_piqi_person conf base p compute_sosa
        else
          Api_util.empty_piqi_person conf ref_person
    | None -> Api_util.empty_piqi_person conf ref_person
  in
  let data = Api_util.data_person p in
  Api_util.print_result conf data

let print_list_ref_person conf base =
  let list_ref_person = Api_util.get_params conf Api_piqi_ext.parse_list_reference_persons in
  let filters = Api_util.get_filters conf in
  let pl =
    List.map
      (fun ref_p ->
        let sn = ref_p.Api_piqi.Reference_person.n in
        let fn = ref_p.Api_piqi.Reference_person.p in
        let occ = ref_p.Api_piqi.Reference_person.oc in
        match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
        | Some ip ->
            let p  = Geneweb.Util.pget conf base ip in
            Api_def.PFull p
        | None -> Api_def.PLight ref_p )
      list_ref_person.Api_piqi.List_reference_persons.list_ref_persons
  in
  let data =
    Api_util.data_list_person_option conf base filters pl
  in
  Api_util.print_result conf data

let print_ref_person_from_ip conf base =
  let id = Api_util.get_params conf Api_piqi_ext.parse_index in
  let ip = Gwdb.iper_of_string @@ Int32.to_string id.Api_piqi.Index.index in
  let ref_p = Api_util.person_to_reference_person base @@ Gwdb.poi base ip in
  let data = Api_piqi_ext.gen_reference_person ref_p in
  Api_util.print_result conf data


(**/**) (* API_FIRST_AVAILABLE_PERSON *)

let print_first_available_person conf base =
  let empty_ref = Api_util.empty_reference_person in
  let continue = ref true in
  let res = ref empty_ref in
  Gwdb.Collection.fold_until (fun () -> !continue) begin fun () p ->
    if Geneweb.Util.is_hide_names conf p || Api_util.is_empty_or_quest_name p ||
       not (Geneweb.Util.authorized_age conf base p)
    then ()
    else
      begin
        res := Api_util.person_to_reference_person base p ;
        continue := false
      end
  end () (Gwdb.persons base) ;
  let data = Api_piqi_ext.gen_reference_person !res in
  Api_util.print_result conf data


(**/**) (* API_SOSA *)

(** [Description] : Cette fonction est utilisée pour la première saisie.
       Elle prend une référence_person et si elle a des enfants, alors on
       renvoi le premier enfant, sinon on renvoi la même personne. *)
let print_find_sosa conf base =
  let ref_person = Api_util.get_params conf Api_piqi_ext.parse_reference_person in
  let n = ref_person.Api_piqi.Reference_person.n in
  let p = ref_person.Api_piqi.Reference_person.p in
  let oc = ref_person.Api_piqi.Reference_person.oc in
  let ref_p =
    match Gwdb.person_of_key base p n (Int32.to_int oc) with
    | Some ip ->
      let arr = Gwdb.get_family (Gwdb.poi base ip) in
      let len = Array.length arr in
      let rec loop i =
        if i < len
        then begin
          let fam = Gwdb.foi base (Array.unsafe_get arr i) in
          match Gwdb.get_children fam with
          | [||] -> loop (i + 1)
          | arr ->
            Api_util.person_to_reference_person base @@ Gwdb.poi base @@ Array.unsafe_get arr 0
        end else
          (* On reconstruit la ref_person pour être sûr des accents. *)
          Api_util.person_to_reference_person base @@ Gwdb.poi base ip
      in
      loop 0
    | None -> Api_util.empty_reference_person
  in
  let data = Api_piqi_ext.gen_reference_person ref_p in
  Api_util.print_result conf data


(**/**) (* API_LAST_MODIFIED_PERSONS *)

let print_last_modified_persons conf base =
  let params = Api_util.get_params conf Api_piqi_ext.parse_last_modifications in
  let filters = Api_util.get_filters conf in
  let wiz =
    match params.Api_piqi.Last_modifications.wizard with
    | Some wiz -> wiz
    | None -> ""
  in
  let max_res =
    match params.Api_piqi.Last_modifications.max_res with
    | Some i -> Int32.to_int i
    | None -> 10
  in
  let range =
    match params.Api_piqi.Last_modifications.range with
    | Some range ->
        let date_begin = range.Api_piqi.Filter_date_range.date_begin in
        let dmy1 =
          Date.{ day = Int32.to_int date_begin.Api_piqi.Filter_date.day;
            month = Int32.to_int date_begin.Api_piqi.Filter_date.month;
            year = Int32.to_int date_begin.Api_piqi.Filter_date.year;
            prec = Sure; delta = 0 }
        in
        let date_end = range.Api_piqi.Filter_date_range.date_end in
        let dmy2 =
          Date.{ day = Int32.to_int date_end.Api_piqi.Filter_date.day;
            month = Int32.to_int date_end.Api_piqi.Filter_date.month;
            year = Int32.to_int date_end.Api_piqi.Filter_date.year;
            prec = Sure; delta = 0 }
        in
        let prec = range.Api_piqi.Filter_date_range.only_exact in
        Some (dmy1, dmy2, prec)
    | None -> None
  in
  let is_time_included time =
    match range with
    | Some (date_begin, date_end, prec) ->
        (* time : 0000-00-00 00:00:00 *)
        let date =
          let y = int_of_string (String.sub time 0 4) in
          let m = int_of_string (String.sub time 5 2) in
          let d = int_of_string (String.sub time 8 2) in
          let dmy =
            Date.{ day = d; month = m; year = y; prec = Sure; delta = 0; }
          in
          Some (Date.Dgreg (dmy, Dgregorian))
        in
        Api_util.is_date_included prec date date_begin date_end
    | None -> true
  in
  let date_before_interval time =
    match range with
    | Some (date_begin, _, prec) ->
      (* time : 0000-00-00 00:00:00 *)
      let date =
        let y = int_of_string (String.sub time 0 4) in
        let m = int_of_string (String.sub time 5 2) in
        let d = int_of_string (String.sub time 8 2) in
        let dmy =
          Date.{ day = d; month = m; year = y; prec = Sure; delta = 0; }
        in
        Some (Date.Dgreg (dmy, Dgregorian))
      in
      let dmy_zero = Date.{ day = 1; month = 1; year = 1970; prec = Sure; delta = 0; } in
      Api_util.is_date_included prec date dmy_zero date_begin
    | None -> true
  in
  let p_mem ip list =
    let rec loop list =
      match list with
      | [] -> false
      | p :: list ->
          if ip = Gwdb.get_iper p then true
          else loop list
    in
    loop list
  in
  let list =
    match
      try Some (Secure.open_in_bin (Geneweb.History.file_name conf))
      with Sys_error _ -> None
    with
    | Some ic ->
        let () = Geneweb.SosaCache.build_sosa_ht conf base in
        let pos = in_channel_length ic in
        let vv = (ref (Bytes.create 0), ref 0) in
        let rec loop list res pos =
          if res = 0 then list
          else
            match
              try Some (Mutil.rev_input_line ic pos vv)
              with End_of_file -> None
            with
            | Some (line, pos) ->
                (match Geneweb.History.line_fields line with
                | Some (time, user, action, keyo) ->
                    if (wiz = "" || user = wiz) then
                      if is_time_included time then
                        match keyo with
                        | Some key ->
                            (match action with
                            | "mn" | "dp" | "cp" | "cs" | "co" ->
                                loop list res pos
                            | _ ->
                                (match Gutil.person_ht_find_all base key with
                                | [ip] ->
                                    let p = Gwdb.poi base ip in
                                    if not (Api_util.is_empty_or_quest_name p) &&
                                      Api_util.apply_filters_p
                                        conf filters (Geneweb.SosaCache.get_sosa_person) p &&
                                      not (p_mem ip list)
                                    then loop (p :: list) (res - 1) pos
                                    else loop list res pos
                                | _ -> loop list res pos))
                        | None -> loop list res pos
                      else
                        if date_before_interval time then list
                        else loop list res pos
                    else loop list res pos
                | None -> loop list res pos)
            | None -> list
        in
        let list = loop [] max_res pos in
        close_in ic;
        List.rev list
    | None -> []
  in
  let data = Api_util.conv_data_list_person conf base filters list in
  Api_util.print_result conf data


(**/**) (* API_LAST_VISITED_PERSONS *)

let print_last_visited_persons conf base =
  let last_visits = Api_util.get_params conf Api_piqi_ext.parse_last_visits in
  let user = last_visits.Api_piqi.Last_visits.user in
  let filters = Api_util.get_filters conf in
  let list =
    if user = "" then []
    else try Hashtbl.find (Geneweb.Util.read_visited conf) user with Not_found -> []
  in
  (* On ne supprime pas le fichier de cache, même après un envoi Gendcom, *)
  (* donc on vérifie que les personnes existent toujours dans la base.    *)
  let list =
    List.fold_right begin fun (ip, _) acc ->
      if Gwdb.iper_exists base ip then
        let p = Gwdb.poi base ip in
        if Api_util.apply_filters_p conf filters (Geneweb.SosaCache.get_single_sosa conf base) p
        then p :: acc
        else acc
      else acc
    end list []
  in
  let data = Api_util.conv_data_list_person conf base filters list in
  Api_util.print_result conf data


(**/**) (* API_MAX_ANCESTORS *)

let print_max_ancestors =
  fun conf base ->
  let ipers = Gwdb.ipers base in
  let ancestors = Gwdb.iper_marker (Gwdb.ipers base) Geneweb.Util.IperSet.empty in
  let mark = Gwdb.iper_marker (Gwdb.ipers base) false in
  let has_children p =
    Array.exists
      (fun ifam -> Array.length (Gwdb.get_children @@ Gwdb.foi base ifam) > 0)
      (Gwdb.get_family p)
  in

  let rec nb_ancestors ip =
    if Gwdb.Marker.get mark ip then Gwdb.Marker.get ancestors ip
    else
      begin
        let anc =
          match Gwdb.get_parents (Gwdb.poi base ip) with
          | Some ifam ->
              let cpl = Gwdb.foi base ifam in
              let anc =
                Geneweb.Util.IperSet.add (Gwdb.get_father cpl) @@ Gwdb.Marker.get ancestors ip
              in
              let anc =
                Geneweb.Util.IperSet.add (Gwdb.get_mother cpl) anc
              in
              let anc2 =
                Geneweb.Util.IperSet.union
                  (nb_ancestors (Gwdb.get_father cpl))
                  (nb_ancestors (Gwdb.get_mother cpl))
              in
              Geneweb.Util.IperSet.union anc anc2
          | None -> Geneweb.Util.IperSet.empty
        in
        Gwdb.Marker.set ancestors ip anc;
        Gwdb.Marker.set mark ip true;
        anc
      end
  in

  Gwdb.Collection.iter begin fun p ->
    if has_children p || Gwdb.Marker.get mark (Gwdb.get_iper p) then ()
    else
      begin
        let i = Gwdb.get_iper p in
        let anc = nb_ancestors i in
        Gwdb.Marker.set ancestors i anc;
        Gwdb.Marker.set mark i true
      end
  end (Gwdb.persons base) ;

  (* ip, nb_anc *)
  let res = ref (Gwdb.dummy_iper, 0) in
  Gwdb.Collection.iter begin fun i ->
    let nb = Geneweb.Util.IperSet.cardinal @@ Gwdb.Marker.get ancestors i in
    if nb > snd !res then res := (i, nb)
  end ipers ;
  let ref_p = Api_util.person_to_reference_person base @@ Gwdb.poi base (fst !res) in
  let data = Api_piqi_ext.gen_reference_person ref_p in
  Api_util.print_result conf data


(**/**) (* API_IMAGE_ALL *)

let print_img_all conf base =
  let filters = Api_util.get_filters conf in
  let aux fp fl =
    let list =
      Gwdb.Collection.fold begin fun acc p ->
          match Geneweb.Image.get_portrait conf base p with
          | Some src -> fp p (Geneweb.Image.src_to_string src) :: acc
          | None -> acc
      end [] (Gwdb.persons base)
    in
    if filters.nb_results then
      let len = Api_piqi.Internal_int32.({value = Int32.of_int (List.length list)}) in
      let data = Api_piqi_ext.gen_internal_int32 len in
      Api_util.print_result conf data
    else
      Api_util.print_result conf (fl list)
  in
  let compute_sosa = Api_util.compute_sosa conf base false in
  if Api_util.p_getenvbin conf.env "full_infos" = Some "1" then
    aux
      (fun p img ->
         let p = Api_util.pers_to_piqi_person_full conf base p compute_sosa in
         Api_piqi.Full_image.({person = p; img = img;}))
      (fun list -> Api_piqi_ext.gen_list_full_images @@ Api_piqi.List_full_images.({images = list}) )
  else
    aux
      (fun p img ->
         let p = Api_util.pers_to_piqi_person_light conf base p compute_sosa in
         Api_piqi.Image.({person = p; img}))
      (fun list ->
         Api_piqi_ext.gen_list_images @@ Api_piqi.List_images.({list_images = list}))

(**/**) (* API_IMAGE_APP *)

let print_img_person conf base =
  let id = Api_util.get_params conf Api_piqi_ext.parse_index in
  let ip = Gwdb.iper_of_string @@ Int32.to_string id.Api_piqi.Index.index in
  let p = Gwdb.poi base ip in
  let img_addr = Api_util.get_portrait conf base p |> Option.value ~default:"" in
  let img_from_ip = Api_piqi.Image_address.({img = img_addr}) in
  let data = Api_piqi_ext.gen_image_address img_from_ip in
  Api_util.print_result conf data



(**/**) (* API_UPDT_IMAGE *)

let print_updt_image conf base =
  let pers_img_l = Api_util.get_params conf Api_piqi_ext.parse_list_pers_img in
  let pers_img_l = pers_img_l.Api_piqi.List_pers_img.list_pers_img in
  List.iter
    (fun pers_img ->
      let pers = pers_img.Api_piqi.Pers_img.person in
      let sn = pers.Api_piqi.Reference_person.n in
      let fn = pers.Api_piqi.Reference_person.p in
      let occ = pers.Api_piqi.Reference_person.oc in
      let img = pers_img.Api_piqi.Pers_img.img in
      match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
      | Some ip ->
          let p = Gwdb.poi base ip in
          let p =
            {(Gwdb.gen_person_of_person p) with image = Gwdb.insert_string base img}
          in
          Gwdb.patch_person base p.key_index p
      | None -> () )
    pers_img_l;
  Gwdb.commit_patches base;
  Api_util.print_result conf (fun _ -> "")

exception WarningFound
let table_contains_warning base tbl w =
  try Hashtbl.iter (fun w' _ ->
          if Geneweb.CheckItem.eq_warning base w w'
          then raise WarningFound) tbl;
      false
  with WarningFound -> true

let print_base_warnings conf base =
  Wserver.set_on_timeout (fun _ ->
      let empty = Api_warnings.empty in
      let data = Api_piqi_ext.gen_base_warnings empty in
      Api_util.print_result conf data
  );
  let filters = Api_util.get_filters conf in
  let errors = ref [] in
  let warnings = Hashtbl.create 0 in
  Geneweb.Check.check_base base
    (fun e -> errors := e :: !errors)
    (fun w ->
       if not @@ table_contains_warning base warnings w
       then Hashtbl.add warnings w true)
    ignore ;
  let data =
    if filters.Api_def.nb_results then
      let len = List.length !errors + Hashtbl.length warnings in
      let len = Api_piqi.Internal_int32.({value = Int32.of_int len}) in
      Api_piqi_ext.gen_internal_int32 len
    else
      let result =
        List.fold_left
          (Api_warnings.add_error_to_piqi_warning_list base)
          Api_warnings.empty
          !errors
      in
      let result =
        (* Make the warning list uniq *)
        Hashtbl.fold begin fun (x : Geneweb.CheckItem.base_warning) _ acc ->
          Api_warnings.add_warning_to_piqi_warning_list conf base acc x
        end warnings result
      in
      Api_piqi_ext.gen_base_warnings result
  in
  Api_util.print_result conf data

let person_warnings conf base p =
  let ws = Geneweb.CheckItem.person_warnings conf base p in
  if List.length ws < 100 then
    List.fold_left begin fun acc x ->
      Api_warnings.add_warning_to_piqi_warning_list conf base acc x
    end Api_warnings.empty ws
  else Api_warnings.empty

let print_person_warnings conf base =
  let ref_person = Api_piqi_util.get_params conf Api_piqi_ext.parse_reference_person_i in
  match
    match ref_person.Api_piqi.Reference_person_i.i with
    | Some i -> Some (Gwdb.iper_of_string i)
    | None ->
      match ref_person.Api_piqi.Reference_person_i.key with
      | Some ref_person ->
        let sn = ref_person.Api_piqi.Reference_person.n in
        let fn = ref_person.Api_piqi.Reference_person.p in
        let occ = ref_person.Api_piqi.Reference_person.oc in
        Gwdb.person_of_key base fn sn (Int32.to_int occ)
      | None -> None
  with
  | None -> assert false
  | Some ip ->
    Geneweb.Util.pget conf base ip
    |> person_warnings conf base
    |> Api_piqi_ext.gen_base_warnings
    |> Api_util.print_result conf

(**/**) (* Récupération de toute une base. *)

let print_all_persons conf base =
  let params = Api_util.get_params conf Api_piqi_ext.parse_all_persons_params in
  let filters = Api_util.get_filters conf in
  let (from, until) =
    match (params.Api_piqi.All_persons_params.from, params.Api_piqi.All_persons_params.limit) with
    | (Some f, Some l) -> (Int32.to_int f, min (Gwdb.nb_of_persons base - 1) (Int32.to_int f + Int32.to_int l))
    | (Some f, None) -> (Int32.to_int f, Gwdb.nb_of_persons base - 1)
    | (None, Some l) -> (0, Int32.to_int l)
    | (None, None) -> (0, Gwdb.nb_of_persons base - 1)
  in
  let () = Geneweb.SosaCache.build_sosa_ht conf base in
  let list =
    Gwdb.Collection.fold ~from ~until (fun acc i ->
        let pass_filters = Api_util.apply_filters_p conf filters Geneweb.SosaCache.get_sosa_person i in
        if pass_filters then i :: acc else acc
      ) [] (Gwdb.persons base)
  in
  let data = Api_util.conv_data_list_person conf base filters list in
  Api_util.print_result conf data

let print_all_families conf base =
  let params = Api_util.get_params conf Api_piqi_ext.parse_all_families_params in
  let filters = Api_util.get_filters conf in
  let from = params.Api_piqi.All_families_params.from in
  let limit = params.Api_piqi.All_families_params.limit in
  let nb_families = Gwdb.nb_of_families base in
  let (from, limit) =
    match (from, limit) with
    | (Some f, Some l) -> (Int32.to_int f, Int32.to_int f + Int32.to_int l)
    | (Some f, None) -> (Int32.to_int f, nb_families)
    | (None, Some l) -> (0, Int32.to_int l)
    | (None, None) -> (0, nb_families)
  in
  let () = Geneweb.SosaCache.build_sosa_ht conf base in
  let len = limit - from in
  let list =
    Gwdb.Collection.fold_until
      (fun (_, n) -> n < len)
      begin fun ((list, n) as acc) i ->
        if n < from then acc
        else (i :: list, n + 1)
      end ([], 0) (Gwdb.ifams base)
  in
  let data =
    if filters.nb_results then
      let len = Api_piqi.Internal_int32.({value = Int32.of_int (List.length @@ fst list)}) in
      Api_piqi_ext.gen_internal_int32 len
    else
      let list = List.map (Api_util.fam_to_piqi_family conf base) (List.rev @@ fst list) in
      let list = Api_piqi.List_full_families.({families = list}) in
      Api_piqi_ext.gen_list_full_families list
  in
  Api_util.print_result conf data


module HistoryApi = struct
  let time_of_string s =
    try
      Scanf.sscanf s "%i-%i-%i %i:%i:%i"
        (fun year month day hour minute second ->
           {Api_piqi.Time.year = Int32.of_int year;
            month = Int32.of_int month;
            day = Int32.of_int day;
            hour = Int32.of_int hour;
            minute = Int32.of_int minute;
            second = Int32.of_int second})
    with Scanf.Scan_failure _ ->
      {Api_piqi.Time.year = Int32.zero;
       month = Int32.zero;
       day = Int32.zero;
       hour = Int32.zero;
       minute = Int32.zero;
       second = Int32.zero}

  let action_of_string = function
    | "ap" -> `person_added
    | "mp" -> `person_modified
    | "dp" -> `person_deleted
    | "fp" -> `person_merged
    | "si" -> `image_received
    | "di" -> `image_deleted
    | "af" -> `family_added
    | "mf" -> `family_modified
    | "df" -> `family_deleted
    | "if" -> `family_inverted
    | "ff" -> `family_merged
    | "cn" -> `changed_children_names
    | "aa" -> `parents_added
    | "mn" -> `notes_modified
    | "cp" -> `place_modified
    | "cs" -> `source_modified
    | "co" -> `occupation_modified
    | s -> raise (Invalid_argument s)

  let person_of_key conf base s =
    let year_of_cdate d = Option.bind (Date.od_of_cdate d) Date.year_of_date in
    let has_history fn sn occ =
      let person_file = Geneweb.HistoryDiff.history_file fn sn occ in
      Sys.file_exists (Geneweb.HistoryDiff.history_path conf person_file)
    in
    let history_person_of_iper ip =
      let pers = Gwdb.poi base ip in
      let lastname = Gwdb.sou base (Gwdb.get_surname pers) in
      let firstname = Gwdb.sou base (Gwdb.get_first_name pers) in
      let oc = Gwdb.get_occ pers in
      let has_history = has_history firstname lastname oc in
      let oc = Int32.of_int oc in
      let n = Name.lower lastname in
      let p = Name.lower firstname in
      let year1 =
        let birth_year = year_of_cdate (Gwdb.get_birth pers) in
        if Option.is_some birth_year then birth_year
        else year_of_cdate (Gwdb.get_baptism pers)
      in
      let year2 =
        let death_year =
          Option.bind (Date.date_of_death (Gwdb.get_death pers)) Date.year_of_date
        in
        if Option.is_some death_year then death_year
        else Option.bind (Date.date_of_burial (Gwdb.get_burial pers)) Date.year_of_date
      in
      {
        Api_piqi.History_person.n;
        p;
        oc;
        firstname;
        lastname;
        year1 = Option.map Int32.of_int year1;
        year2 = Option.map Int32.of_int year2;
        exists_in_base = true;
        has_history;
      }
    in
    let history_person_of_key (p, oc, n) =
      {
        Api_piqi.History_person.n;
        p;
        oc = Int32.of_int oc;
        firstname = "";
        lastname = "";
        year1 = None;
        year2 = None;
        exists_in_base = false;
        has_history = false;
      }
    in
    match Gutil.person_of_string_key base s with
    | Some ip -> Some (history_person_of_iper ip)
    | None -> Gutil.split_key s |> Option.map history_person_of_key

  let note_link conf key =
    let pg, part =
      let i, j =
        try
          let i = String.rindex key '/' in
          (i, i + 1)
        with Not_found -> (0, 0)
      in
      let pg = String.sub key 0 i in
      let s = String.sub key j (String.length key - j) in
      try pg, Some (int_of_string s)
      with Failure _ -> key, None
    in
    let link_parameters = "m=NOTES" in
    let link_parameters =
      if pg <> "" then
        Printf.sprintf {|%s;f=%s|} link_parameters pg
      else link_parameters in
    let link_parameters =
      if Option.is_some part then
        Printf.sprintf {|%s;v=%d|} link_parameters (Option.get part)
      else link_parameters in
    let link_txt =
      if pg <> "" then
        Printf.sprintf {|[%s]|} pg
      else Geneweb.Util.transl_nth conf "note/notes" 1 in
    {
      Api_piqi.History_note.link_parameters;
      link_txt;
    }

  let history_entry conf base time user action keyo =
    let time = time_of_string time in
    let action = action_of_string action in
    let person, note =
      if action = `notes_modified then
        let note = Option.map (note_link conf) keyo in
        None, note
      else
        let person = Option.bind keyo (person_of_key conf base) in
        person, None
    in
    {
      Api_piqi.History_entry.modification_type = action;
      time;
      editor = user;
      person;
      note;
    }

  let history_list conf base page elements_per_page filter_user =
    let f ~time ~user ~action ~keyo =
      history_entry conf base time user action keyo
    in
    let ipage = Int32.to_int page in
    let elements_per_page = Int32.to_int elements_per_page in
    let filter = match filter_user with
      | Some filter_user ->
        fun ~time:_ ~user ~action:_ ~keyo:_ -> filter_user = user
      | None ->
        fun ~time:_ ~user:_ ~action:_ ~keyo:_ -> true
    in
    let entries = Geneweb.History.filter_map_history
        ~conf
        ~skip:((pred ipage) * elements_per_page)
        ~n:elements_per_page
        ~filter
        ~f
    in
    let total_elements = Int32.of_int (Geneweb.History.total_entries ~conf ~filter) in
    Api_piqi_ext.gen_history {Api_piqi.History.entries; page; total_elements}
end

let history conf base =
  let params = Api_util.get_params conf Api_piqi_ext.parse_history_request in
  let page = params.Api_piqi.History_request.page in
  let elements_per_page = params.Api_piqi.History_request.elements_per_page in
  let filter_user = params.Api_piqi.History_request.filter_user in
  let data = HistoryApi.history_list conf base page elements_per_page filter_user in
  Api_util.print_result conf data
