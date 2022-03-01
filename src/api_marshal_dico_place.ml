
module StrSet = Set.Make (String)

let quote s =  "\"" ^ s ^ "\""
              
let build_line =
  let rec aux s l = match l with
    | [x] -> s ^ (quote x)
    | x :: xs -> aux (s ^ (quote x) ^ ",") xs
    | [] -> s
  in fun l -> aux "" l
            
let add_opt l set = match l with
  | x :: _ when x <> "" ->
     StrSet.add (build_line l) set
  | _ -> set

let generate assets lang k data =
  match Api_search.dico_fname assets lang k with
  | None -> ()
  | Some fname_set ->
     let ext_flags =
       [ Open_wronly ; Open_append ; Open_creat ; Open_binary ; Open_nonblock ]
     in
     let oc = open_out_gen ext_flags 0o644 fname_set in
     output_value oc (data : Api_search.dico) ;
     close_out oc

let sorted_array_of_set s =
  let a = StrSet.elements s |> Array.of_list in
  Array.sort Gutil.alphabetic a ;
  a
       
let write_dico_place_set ~assets ~fname_csv ~lang =
  !Geneweb.GWPARAM.syslog `LOG_DEBUG ("writing places files for lang "
                                      ^ lang ^ " from file: " ^ fname_csv);

  let csv = Api_csv.load_from_file ~file:fname_csv in
  
  let sets = StrSet.empty,
             StrSet.empty,
             StrSet.empty,
             StrSet.empty,
             StrSet.empty
  in
  
  let towns, area_codes, countys, regions, countrys =
    Api_csv.fold_left (
        fun (towns, area_codes, countys, regions, countrys) ->
        function
        | [ _ ; _ ; _ ; _ ; _ ] as l ->
           let towns = add_opt l towns in
           let l = List.tl l in
           let area_codes = add_opt l area_codes in
           let l = List.tl l in
           let countys = add_opt l countys in
           let l = List.tl l in
           let regions = add_opt l regions in
           let l = List.tl l in
           let countrys = add_opt l countrys in
           (towns, area_codes, countys, regions, countrys)
        | l ->
           !Geneweb.GWPARAM.syslog `LOG_DEBUG ("malformed line in file: " ^ fname_csv);
           let s = List.fold_left (fun s a -> s ^ "," ^ a) "" l in
           !Geneweb.GWPARAM.syslog `LOG_DEBUG ("line is: " ^ s);
           (towns, area_codes, countys, regions, countrys)
      ) sets csv
  in
  
  let generate = generate assets lang in
  
  generate `town (sorted_array_of_set towns) ;
  generate `area_code (sorted_array_of_set area_codes) ;
  generate `county (sorted_array_of_set countys) ;
  generate `region (sorted_array_of_set regions) ;
  generate `country (sorted_array_of_set countrys)
