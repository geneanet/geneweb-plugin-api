
let () = print_endline "YAYYYYYYYYYYYYYYYYYYYYYYYYY";


module Csv : sig

  type t

  val load_from_file : file:string -> t

  val fold_left : ('a -> string list -> 'a) -> 'a -> t -> 'a

end = struct

  type t = Csv.t

  let load_from_file ~file =
    Csv.load file


  let fold_left = List.fold_left
    
end





         
module StrSet = Set.Make (String)
(*
let rest str =
  let i = String.index str ',' in
  String.sub str (i + 1) (String.length str - i - 1)

let write_dico_place_set assets fname_csv lang =
  let (towns, area_codes, countys, regions, countrys) =
    let ic = open_in fname_csv in
    let string_set_town = ref StrSet.empty in
    let string_set_area_code = ref StrSet.empty in
    let string_set_county = ref StrSet.empty in
    let string_set_region = ref StrSet.empty in
    let string_set_country = ref StrSet.empty in
    let string_set r s = r := StrSet.add s !r in
    begin
      try while true do
          let line = input_line ic in
          match String.split_on_char ',' line with
          | [ town ; area_code ; county ; region ; country ] ->
            let place = line in
            if town <> "" then string_set string_set_town place ;
            let place = rest place in
            if area_code <> "" then string_set string_set_area_code place ;
            let place = rest place in
            if county <> "" then string_set string_set_county place ;
            let place = rest place in
            if region <> "" then string_set string_set_region place ;
            let place = rest place in
            if country <> "" then string_set string_set_country place ;
          | _ -> ()
        done
      with End_of_file -> close_in ic
    end ;
    let aux r =
      let a = StrSet.elements !r |> Array.of_list in
      Array.sort Gutil.alphabetic a ;
      a
    in
    ( aux string_set_town
    , aux string_set_area_code
    , aux string_set_county
    , aux string_set_region
    , aux string_set_country
    )
  in
  let generate k data =
    match Api_search.dico_fname assets lang k with
    | None -> ()
    | Some fname_set ->
      let ext_flags =
        [ Open_wronly ; Open_append ; Open_creat ; Open_binary ; Open_nonblock ]
      in
      let oc = open_out_gen ext_flags 0o644 fname_set in
      output_value oc (data : Api_search.dico) ;
      close_out oc
  in
  generate `town towns ;
  generate `area_code area_codes ;
  generate `county countys ;
  generate `region regions ;
  generate `country countrys
 *)



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

       
let write_dico_place_set ~assets ~fname_csv ~lang =
  print_endline "======================WRITE DICO========================";
  let csv = Csv.load_from_file ~file:fname_csv in
  let sets = StrSet.empty,
             StrSet.empty,
             StrSet.empty,
             StrSet.empty,
             StrSet.empty
  in
  
  let towns, area_codes, countys, regions, countrys =
    Csv.fold_left (
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
        | _ -> (towns, area_codes, countys, regions, countrys)
      ) sets csv
  in
  
  let aux r =
    let a = StrSet.elements r |> Array.of_list in
    Array.sort Gutil.alphabetic a ;
    a
  in
  let generate = generate assets lang in
  generate `town (aux towns) ;
  generate `area_code (aux area_codes) ;
  generate `county (aux countys) ;
  generate `region (aux regions) ;
  generate `country (aux countrys)
