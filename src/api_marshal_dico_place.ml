
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

module PlacesData : sig
  type t
  val empty : t
  val add_town : t -> string list -> t
  val add_area_code : t -> string list -> t
  val add_county : t -> string list -> t
  val add_region : t -> string list -> t
  val add_country : t -> string list -> t
  val get_towns : t -> StrSet.t
  val get_area_codes : t -> StrSet.t
  val get_counties : t -> StrSet.t
  val get_regions : t -> StrSet.t
  val get_countries : t -> StrSet.t
end = struct
  type t = {
    towns : StrSet.t;
    area_codes : StrSet.t;
    counties : StrSet.t;
    regions : StrSet.t;
    countries : StrSet.t;
  }
  let empty = {
    towns = StrSet.empty;
    area_codes = StrSet.empty;
    counties = StrSet.empty;
    regions = StrSet.empty;
    countries = StrSet.empty;
  }
  let add_town t town =
    let towns = add_opt town t.towns in
    {t with towns}
  let add_area_code t area_code =
    let area_codes = add_opt area_code t.area_codes in
    {t with area_codes}
  let add_county t county =
    let counties = add_opt county t.counties in
    {t with counties}
  let add_region t region =
    let regions = add_opt region t.regions in
    {t with regions}
  let add_country t country =
    let countries = add_opt country t.countries in
    {t with countries}
  let get_towns t = t.towns
  let get_area_codes t = t.area_codes
  let get_counties t =  t.counties
  let get_regions t = t.regions
  let get_countries t = t.countries
end

let write_dico_place_set ~assets ~fname_csv ~lang =
  !Geneweb.GWPARAM.syslog `LOG_DEBUG ("writing places files for lang "
                                      ^ lang ^ " from file: " ^ fname_csv);

  let csv = Api_csv.load_from_file ~file:fname_csv in
  
  let data = PlacesData.empty in
  
  let data =
    Api_csv.fold_left (
      fun data ->
        function
        | [ town ; area_code ; county ; region ; country  ; country_code ] ->
          let data = PlacesData.add_town data [town; area_code; county; region; country; country_code] in
          let data = PlacesData.add_area_code data [area_code; county; region; country; country_code] in
          let data = PlacesData.add_county data [county; region; country; country_code] in
          let data = PlacesData.add_region data [region; country; country_code] in
          let data = PlacesData.add_country data [country; country_code] in
          data
        | l ->
          !Geneweb.GWPARAM.syslog `LOG_DEBUG ("malformed line in file: " ^ fname_csv);
          let s = List.fold_left (fun s a -> s ^ "," ^ a) "" l in
          !Geneweb.GWPARAM.syslog `LOG_DEBUG ("line is: " ^ s);
          data
      ) data csv
  in
  
  let generate = generate assets lang in
  
  generate `town (sorted_array_of_set (PlacesData.get_towns data)) ;
  generate `area_code (sorted_array_of_set (PlacesData.get_area_codes data)) ;
  generate `county (sorted_array_of_set (PlacesData.get_counties data)) ;
  generate `region (sorted_array_of_set (PlacesData.get_regions data)) ;
  generate `country (sorted_array_of_set (PlacesData.get_countries data))
