(**/**)

let cache_file_of_cache_data base_file = function
  | `lastname -> Caches.lastname_cache_fname base_file
  | `firstname -> Caches.first_name_cache_fname base_file
  | `place -> Caches.place_cache_fname base_file
  | `source -> Caches.source_cache_fname base_file
  | `occupation -> Caches.occupation_cache_fname base_file

let has_cache ~conf ~mode =
  let base_file = Geneweb.GWPARAM.bpath (conf.Geneweb.Config.bname ^ ".gwb") in
  let file = cache_file_of_cache_data base_file mode in
  Sys.file_exists file

let starts_with ~ini ~candidate =
  Utf8.start_with_wildcard ini 0 (Name.lower candidate)

let is_valid_suggestion ~mode ~place_mode = match mode, place_mode with
  | `place, (None | Some (`area_code | `country | `county | `region | `town)) ->
    fun ~ini ~candidate ->
      let suburb, no_suburb = Geneweb.Place.split_suburb candidate in
      if suburb <> "" then
        starts_with ~ini ~candidate:suburb || starts_with ~ini ~candidate:no_suburb || starts_with ~ini ~candidate
      else
        starts_with ~ini ~candidate
  | (`source | `occupation | `firstname | `lastname | `place), _ ->
    starts_with

let add_if_valid base filter istr list =
  let s = Gwdb.sou base istr in
  if filter s then s :: list
  else list

let add_places_of_person base filter l person =
  let l = add_if_valid base filter (Gwdb.get_birth_place person) l in
  let l = add_if_valid base filter (Gwdb.get_death_place person) l in
  let l = add_if_valid base filter (Gwdb.get_baptism_place person) l in
  let l = add_if_valid base filter (Gwdb.get_burial_place person) l in
  List.fold_left (fun l pe ->
      add_if_valid base filter (Gwdb.get_pevent_place pe) l
    ) l (Gwdb.get_pevents person)

let add_places_of_family base filter l family =
  let l = add_if_valid base filter (Gwdb.get_marriage_place family) l in
  List.fold_left (fun l fe ->
      add_if_valid base filter (Gwdb.get_fevent_place fe) l
    ) l (Gwdb.get_fevents family)

let add_sources_of_person base filter l person =
  let l = add_if_valid base filter (Gwdb.get_birth_src person) l in
  let l = add_if_valid base filter (Gwdb.get_death_src person) l in
  let l = add_if_valid base filter (Gwdb.get_baptism_src person) l in
  let l = add_if_valid base filter (Gwdb.get_burial_src person) l in
  List.fold_left (fun l pe ->
      add_if_valid base filter (Gwdb.get_pevent_src pe) l
    ) l (Gwdb.get_pevents person)

let add_sources_of_family base filter l family =
  let l = add_if_valid base filter (Gwdb.get_marriage_src family) l in
  List.fold_left (fun l fe ->
      add_if_valid base filter (Gwdb.get_fevent_src fe) l
    ) l (Gwdb.get_fevents family)

let add_lastname_of_person base filter l person =
  add_if_valid base filter (Gwdb.get_surname person) l

let add_first_name_of_person base filter l person =
  add_if_valid base filter (Gwdb.get_first_name person) l

let add_occupation_of_person base filter l person =
  add_if_valid base filter (Gwdb.get_occupation person) l

let complete_with_persons_patch mode base filter data =
  let persons = Gwdb.persons_from_patch base in
  match mode with
  | `lastname ->
    Gwdb.Collection.fold (add_lastname_of_person base filter) data persons
  | `firstname ->
    Gwdb.Collection.fold (add_first_name_of_person base filter) data persons
  | `place ->
    Gwdb.Collection.fold (add_places_of_person base filter) data persons
  | `source ->
    Gwdb.Collection.fold (add_sources_of_person base filter) data persons
  | `occupation ->
    Gwdb.Collection.fold (add_occupation_of_person base filter) data persons

let complete_with_families_patch mode base filter data =
  match mode with
  | `place ->
    Gwdb.Collection.fold (add_places_of_family base filter)
      data
      (Gwdb.families_from_patch base)
  | `source ->
    Gwdb.Collection.fold (add_sources_of_family base filter)
      data
      (Gwdb.families_from_patch base)
  |`lastname | `firstname | `occupation -> data

let complete_with_patch mode base filter data =
  let data = complete_with_persons_patch mode base filter data in
  complete_with_families_patch mode base filter data

let get_list_from_cache ~conf ~base ~mode ~place_mode ~n ~ini =
  let bfile = Geneweb.GWPARAM.bpath (conf.Geneweb.Config.bname ^ ".gwb") in
  let cache_file = cache_file_of_cache_data bfile mode in
  let cache =
    let ic = Secure.open_in_bin cache_file in
    try (Marshal.from_channel ic : string list)
    with
    | e ->
      Geneweb.GWPARAM.syslog `LOG_ERR
        (Printf.sprintf "Error while reading api autocomplete cache %s %s"
           cache_file (Printexc.to_string e));
      close_in ic;
      []
  in
  let ini = Name.lower @@ Ext_string.tr '_' ' ' ini in
  let is_valid = is_valid_suggestion ~mode ~place_mode in
  (* optim : on sait que la liste est triée. *)
  let rec loop list accu nb_res =
    match list with
    | [] -> List.rev accu
    | name :: l ->
      let k = Ext_string.tr '_' ' ' name in
      let (accu, nb_res) =
        if is_valid ~ini ~candidate:k
        then name :: accu, nb_res + 1
        else accu, nb_res
      in
      if nb_res < n then loop l accu nb_res
      else List.rev accu
  in
  let data = loop cache [] 0 in
  let patch_data = complete_with_patch mode base (fun s -> is_valid ~ini ~candidate:s) data in
  Ext_list.take (List.sort_uniq Utf8.alphabetic_order patch_data) n
