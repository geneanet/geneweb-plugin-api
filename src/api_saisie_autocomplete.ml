(**/**)

let cache_file_of_cache_data base_file = function
  | `lastname -> Filename.concat base_file "cache_surname"
  | `firstname -> Filename.concat base_file "cache_first_name"
  | `place -> Filename.concat base_file "cache_place"
  | `source -> Filename.concat base_file "cache_src"
  | `occupation -> Filename.concat base_file "cache_occupation"

let has_cache conf mode =
  let base_file = Geneweb.Util.bpath (conf.Geneweb.Config.bname ^ ".gwb") in
  let file = cache_file_of_cache_data base_file mode in
  Sys.file_exists file

let get_list_from_cache conf mode max_res s =
  let bfile = Geneweb.Util.bpath (conf.Geneweb.Config.bname ^ ".gwb") in
  let cache_file = cache_file_of_cache_data bfile mode in
  let cache =
    let ic = Secure.open_in_bin cache_file in
    try (Marshal.from_channel ic : string list)
    with
    | _ ->
      close_in ic;
      []
  in
  let ini = Name.lower @@ Ext_string.tr '_' ' ' s in
  (* optim : on sait que la liste est triée. *)
  let rec loop list accu nb_res =
    match list with
    | [] -> List.rev accu
    | name :: l ->
      let k = Ext_string.tr '_' ' ' name in
      let (accu, nb_res) =
        if Utf8.start_with_wildcard ini 0 (Name.lower k)
        then name :: accu, nb_res + 1
        else accu, nb_res
      in
      if nb_res < max_res then loop l accu nb_res
      else List.rev accu
  in
  loop cache [] 0

