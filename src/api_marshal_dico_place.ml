
module StrSet = Set.Make (String)
              
module ApiCsv : sig

  type t

  val load_from_file : file:string -> t

  val fold_left : ('a -> string list -> 'a) -> 'a -> t -> 'a

end = struct

  type t = Csv.t

  let load_from_file ~file =
    try
      Csv.load file
    with Csv.Failure (nrecord, nfield, msg) ->
      Geneweb.GWPARAM.Default.syslog
        `LOG_ERR
        ("failed to load csv from :" ^ file
         ^ " " ^ string_of_int nrecord
         ^ " " ^ string_of_int nfield
         ^ " " ^ msg);
      []


  let fold_left = List.fold_left
    
end



let build_line =
  let rec aux s l = match l with
    | [x] -> x
    | x :: xs -> aux (s ^ x ^ ",") xs
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
  Geneweb.GWPARAM.Default.syslog `LOG_DEBUG ("======================WRITE DICO " ^ lang ^ " from " ^ fname_csv ^ "========================");
  (*print_endline "======================WRITE DICO========================";*)

  let csv = ApiCsv.load_from_file ~file:fname_csv in
  
  let sets = StrSet.empty,
             StrSet.empty,
             StrSet.empty,
             StrSet.empty,
             StrSet.empty
  in
  
  let towns, area_codes, countys, regions, countrys =
    ApiCsv.fold_left (
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
           Geneweb.GWPARAM.Default.syslog `LOG_DEBUG ("=================MALFORMED LINE " ^ fname_csv ^ "===================");
           let s = List.fold_left (fun s a -> s ^ "|" ^ a) "" l in
           Geneweb.GWPARAM.Default.syslog `LOG_DEBUG s;
           (towns, area_codes, countys, regions, countrys)
      ) sets csv
  in
  
  let generate = generate assets lang in
  
  generate `town (sorted_array_of_set towns) ;
  generate `area_code (sorted_array_of_set area_codes) ;
  generate `county (sorted_array_of_set countys) ;
  generate `region (sorted_array_of_set regions) ;
  generate `country (sorted_array_of_set countrys)
