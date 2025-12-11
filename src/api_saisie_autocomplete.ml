(**/**)

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

let get_list_from_cache ~conf ~base ~mode ~place_mode ~n ~ini =
  let cache = Caches.read_cache ~conf mode in
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
  let patch_data = Caches.complete_with_patch mode base (fun s -> is_valid ~ini ~candidate:s) data in
  Ext_list.take (List.sort_uniq Utf8.alphabetic_order patch_data) n
