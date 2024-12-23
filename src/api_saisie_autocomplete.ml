(**/**)

let cache_file_of_cache_data base_file = function
  | `lastname -> Caches.lastname_cache_fname base_file
  | `firstname -> Caches.first_name_cache_fname base_file
  | `place -> Caches.place_cache_fname base_file
  | `source -> Caches.source_cache_fname base_file
  | `occupation -> Caches.occupation_cache_fname base_file

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
    | e ->
      Geneweb.GWPARAM.syslog `LOG_ERR
        (Printf.sprintf "Error while reading api autocomplete cache %s %s"
           cache_file (Printexc.to_string e));
      close_in ic;
      []
  in
  let ini = Geneweb_util.Name.lower @@ Geneweb_util.Ext_string.tr '_' ' ' s in
  (* optim : on sait que la liste est triée. *)
  let rec loop list accu nb_res =
    match list with
    | [] -> List.rev accu
    | name :: l ->
      let k = Geneweb_util.Ext_string.tr '_' ' ' name in
      let (accu, nb_res) =
        if Geneweb_util.Utf8.start_with_wildcard ini 0 (Geneweb_util.Name.lower k)
        then name :: accu, nb_res + 1
        else accu, nb_res
      in
      if nb_res < max_res then loop l accu nb_res
      else List.rev accu
  in
  loop cache [] 0

