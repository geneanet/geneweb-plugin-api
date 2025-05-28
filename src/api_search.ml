let append l1 l2 =
  List.fold_left (fun l hd -> hd :: l) l2 (List.rev l1)

let string_start_with ini s = Utf8.start_with_wildcard ini 0 s

(* Algo de Knuth-Morris-Pratt *)
let init_next p =
  let m = String.length p in
  let next = Array.make m 0 in
  let i = ref 1 and j = ref 0 in
  while !i < m - 1 do
    if p.[!i] = p.[!j] then begin incr i; incr j; next.(!i) <- !j end
    else if !j = 0 then begin incr i; next.(!i) <- 0 end
    else j := next.(!j)
  done;
  next

(* Algo de Knuth-Morris-Pratt *)
let kmp p s =
  let p = (Name.lower p) in
  let s = (Name.lower s) in
  (* Optimisation pour GeneWeb *)
  if s = "" || s = "?" then false
  else
    begin
      let next = init_next p and m = String.length p in
      let n = String.length s and i = ref 0 and j = ref 0 in
      while !j < m && !i < n do
        if s.[!i] = p.[!j] then begin incr i; incr j end else
        if !j = 0 then incr i else j := next.(!j)
      done;
      if !j >= m then true else false
    end

(* FIXME: DUPLICATE OF ALLN.SELECT ??? *)
(** [Description] : Fonction qui scanne l'ensemble des noms de la base
    pour une lettre donnée et retourne une liste de personne.
    [Args] :
      - conf            : configuration de la base
      - base            : base de donnée
      - is_surnames     : si True recherche sur les noms de famille
      - ini_p           : début du nom
      - ini_n           : début du prénom
      - need_whole_list : si True, remonte tout
      - letter          : la première lettre des noms/prénoms concernés
    [Retour] :
      - ListPersons : Retourne une liste de personnes.
                                                                              *)
let get_list_of_select_start_with (conf : Geneweb.Config.config) (base : Gwdb.base) (ini_n : string) (ini_p : string) (letter : string) =
    let name =
    (* Si le nom est défini, on parcourt un tableau de noms *)
    if "" <> ini_n
    then
        Gwdb.persons_of_surname base
    else
        (* Sinon, on parcourt un tableau de prénoms *)
        Gwdb.persons_of_first_name base
    in
    try
      (* Itère sur chaque entrée du tableau qui commence par la lettre letter *)
      let istr = Gwdb.spi_first name letter in
        let rec loop istr list =
          let s = Mutil.nominative (Gwdb.sou base istr) in
          let k = Geneweb.Util.name_key base s in
            (* Vérifie que le début du nom de famille de la personne correspond à celui demandé *)
            if "" = ini_n || string_start_with (Name.lower ini_n) (Name.lower k) then
              let list =
                if s <> "?" then
                  let my_list = Gwdb.spi_find name istr in
                  let my_list =
                    List.fold_left
                     (fun l ip ->
                        let p = Gwdb.poi base ip in
                        let isn = Gwdb.get_surname p in
                        if "" <> ini_n
                        then
                            if Gwdb.eq_istr isn istr then
                                if "" <> ini_p
                                then
                                    let isp = Gwdb.sou base (Gwdb.get_first_name p) in
                                    if Gwdb.eq_istr isn istr && string_start_with (Name.lower ini_p) (Name.lower isp)
                                    then
                                        (* Prénom===Prénom && Nom===Nom *)
                                        (ip :: l)
                                    else l
                                else
                                    (* Prénom===* && Nom===Nom *)
                                    (ip :: l)
                            else l
                        else
                            if "" <> ini_p
                            then
                                let isp = Gwdb.sou base (Gwdb.get_first_name p) in
                                if string_start_with (Name.lower ini_p) (Name.lower isp)
                                then
                                    (* Prénom===Prénom && Nom===* *)
                                    (ip :: l)
                                else l
                            else
                                (* Prénom===* && Nom===* *)
                                (ip :: l)
                        )
                     [] my_list
                  in
                  let my_list =
                    if conf.Geneweb.Config.use_restrict then
                      List.fold_left
                        (fun l ip ->
                          if Geneweb.Util.is_restricted conf base ip then l
                           else (ip :: l) )
                        [] my_list
                    else my_list
                  in
                  (* Ajoute à list les personnes trouvées *)
                  List.rev_append my_list list
                (* Sort totalement de l'itération puisque les personnes ne sont plus définies *)
                else list
              in
              match Gwdb.spi_next name istr with
              | istr -> loop istr list
              | exception Not_found -> list
            else
              match Gwdb.spi_next name istr with
              | istr -> loop istr list
              | exception Not_found -> list
        (* Première itération, on initialise au passage list comme tableau vide *)
        in loop istr []
    with Not_found -> []

(** [Description] : Retourne une liste de personne dont le nom OU le prénom
    commence par 'ini_n' ou 'ini_p'.
    [Args] :
      - conf            : configuration de la base
      - base            : base de donnée
      - ini_n           : début du nom à chercher
      - ini_p           : début du nom à chercher
      - need_whole_list : si True, remonte tout
    [Retour] :
      - ListPersons : Retourne une liste de personnes.
                                                                              *)
let select_start_with (conf : Geneweb.Config.config) (base : Gwdb.base) (ini_n : string) (ini_p : string) =
  let ini_n = Geneweb.Util.name_key base ini_n in
  let start =
    if ini_n <> ""
    then
      Ext_string.tr '_' ' ' ini_n
    else
      Ext_string.tr '_' ' ' ini_p
  in
  let list_min =
    let letter = String.lowercase_ascii (String.sub start 0 1) in
    get_list_of_select_start_with conf base ini_n ini_p letter
  in
  let list_maj =
    let letter = String.uppercase_ascii (String.sub start 0 1) in
    get_list_of_select_start_with conf base ini_n ini_p letter
  in
  List.rev_append list_maj list_min

let aux_ini (s : string) : string list =
  let rec loop (s : Adef.encoded_string) acc =
    if String.contains (s :> string) '+' then
      let index = String.index (s :> string) '+' in
      let start = index + 1 in
      let len = String.length (s :> string) - start in
      let ns = Adef.encoded @@ String.sub (s :> string) start len in
      loop ns (Mutil.decode (Adef.encoded @@ String.sub (s :> string) 0 index) :: acc)
    else (Mutil.decode s :: acc)
  in
  loop (Mutil.encode s) []

let select_both_all base ini_n ini_p maiden_name =
  let find_sn p x = kmp x (Gwdb.sou base (Gwdb.get_surname p)) in
  let find_fn p x = kmp x (Gwdb.sou base (Gwdb.get_first_name p)) in
  let find_str s x = kmp x s in
  let ini_n = Geneweb.Util.name_key base ini_n in
  let ini_n = aux_ini ini_n in
  let ini_n = List.filter (fun s -> s <> "") ini_n in
  (* choper dans code varenv la variable qui dit que c'est + *)
  let ini_p = aux_ini ini_p in
  let add_maiden p ini_p l =
    if Gwdb.get_sex p = Def.Male then
      l :=
        Array.fold_left
          (fun acc ifam ->
             let fam = Gwdb.foi base ifam in
             let ip = Gutil.spouse (Gwdb.get_iper p) fam in
             let sp = Gwdb.poi base ip in
             if List.for_all (fun s -> find_fn sp s) ini_p then ip :: acc else acc)
          !l (Gwdb.get_family p)
  in
  let add_maiden2 p ini_n ini_p l =
    (* On sépare les noms avec tirets ... *)
    let ini_n =
      List.fold_left
        (fun accu s -> List.rev_append (String.split_on_char '-' s) accu)
        [] ini_n
    in
    if Gwdb.get_sex p = Def.Male && List.exists (fun s -> find_sn p s) ini_n
    then
      Array.iter
        (fun ifam ->
           let fam = Gwdb.foi base ifam in
           let ip = Gutil.spouse (Gwdb.get_iper p) fam in
           let sp = Gwdb.poi base ip in
           let names =
             Gwdb.sou base (Gwdb.get_surname p) ^ " " ^ Gwdb.sou base (Gwdb.get_surname sp)
           in
           if List.for_all (fun s -> find_str names s) ini_n
           then add_maiden p ini_p l)
        (Gwdb.get_family p)
  in
  let list = ref [] in
  Gwdb.Collection.iter begin fun p ->
    let ip = Gwdb.get_iper p in
    if List.for_all (fun s -> find_sn p s) ini_n then
      begin
        if List.for_all (fun s -> find_fn p s) ini_p then
          list := ip :: !list
        else if maiden_name then add_maiden p ini_p list
      end
    else
      (* On cherche une partie du nom de jeune fille dans les noms donnés. *)
      if maiden_name then add_maiden2 p ini_n ini_p list
  end (Gwdb.persons base) ;
  !list

let select_all base is_surnames ini =
  let find p x =
    if is_surnames then kmp x (Gwdb.sou base (Gwdb.get_surname p))
    else kmp x (Gwdb.sou base (Gwdb.get_first_name p))
  in
  let ini =
    if is_surnames then Geneweb.Util.name_key base ini
    else ini
  in
  let ini = aux_ini ini in
  let list = ref [] in
  Gwdb.Collection.iter begin fun p ->
    if List.for_all (fun s -> find p s) ini
    then list := Gwdb.get_iper p :: !list
  end (Gwdb.persons base) ;
  !list

let print_list conf base filters list =
  let person_l =
    Gwdb.PersonSet.elements
      (List.fold_left
         begin fun acc p ->
           let p = Gwdb.poi base p in
           if Api_util.apply_filters_p conf filters
               (fun person -> Geneweb.Sosa_cache.get_sosa_person ~conf ~base ~person)
               p
           then Gwdb.PersonSet.add p acc
           else acc
         end
         Gwdb.PersonSet.empty list)
  in
  let person_l =
    if filters.nb_results then person_l
    else
      List.sort
        (fun p1 p2 ->
          let sn1 = Name.lower (Gwdb.p_surname base p1) in
          let sn2 = Name.lower (Gwdb.p_surname base p2) in
          let comp = Utf8.alphabetic_order sn1 sn2 in
          if comp = 0 then
            let fn1 = Name.lower (Gwdb.p_first_name base p1) in
            let fn2 = Name.lower (Gwdb.p_first_name base p2) in
            Utf8.alphabetic_order fn1 fn2
          else comp)
        person_l
  in
  let data = Api_util.conv_data_list_person conf base filters person_l in
  Api_util.print_result conf data

(*
   La différence entre la recherche approximative et lastname_or_surname est
   si on cherche un nom ET un prénom (dans les autres cas, on obtient les
   mêmes résultats.
   De ce fait, on utilise list_n = select_all n, list_p = select_all p et on
   fait l'union des deux ce qui est beaucoup plus efficace.
*)
let print_search conf base =
  let search_params = Api_util.get_params conf (fun x f -> Api_piqi_ext.parse_search_params x f) in
  let filters = Api_util.get_filters conf in
  match
     (search_params.Api_piqi.Search_params.lastname,
      search_params.Api_piqi.Search_params.firstname)
  with
   | (Some n, Some fs) ->
      let _ = Gwdb.load_strings_array base in
      let list =
        if Name.lower n = "" && Name.lower fs = "" then
          []
        else
          let maiden_name = search_params.Api_piqi.Search_params.maiden_name in
          match search_params.Api_piqi.Search_params.search_type with
          | `starting_with -> select_start_with conf base n fs
          | `approximative -> select_both_all base n fs maiden_name
          | `lastname_or_firstname ->
               let list_n = select_all base true n in
               let list_p = select_all base false fs in
               List.rev_append list_n list_p
      in
      print_list conf base filters list
  | (Some n, None) ->
      let _ = Gwdb.load_strings_array base in
      let list =
        if Name.lower n = "" then
          []
        else
          match search_params.Api_piqi.Search_params.search_type with
          | `starting_with -> select_start_with conf base n ""
          | `approximative -> select_all base true n
          | `lastname_or_firstname -> select_all base true n
      in
      print_list conf base filters list
  | (None, Some fs) ->
      let _ = Gwdb.load_strings_array base in
      let list =
        if Name.lower fs = "" then
          []
        else
          match search_params.Api_piqi.Search_params.search_type with
          | `starting_with -> select_start_with conf base "" fs
          | `approximative -> select_all base false fs
          | `lastname_or_firstname -> select_all base false fs
      in
      print_list conf base filters list
  | (None, None) -> ()



(**/**) (* Recherche utilisée pour l'auto-completion ou relier personne. *)

let rec skip_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i = ' ' then skip_spaces x (i + 1)
  else i

let rec skip_no_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i != ' ' then skip_no_spaces x (i + 1)
  else i

let string_incl_start_with x y =
  let rec loop j_ini =
    if j_ini = String.length y then false
    else
      let rec loop1 i j =
        if i = String.length x then true
        else if j = String.length y then
          if x.[i] = '_' then loop1 (i + 1) j
          else false
        else if y.[j] = x.[i] || y.[j] = ' ' && x.[i] = '_' then
          loop1 (i + 1) (j + 1)
        else loop (skip_spaces y (skip_no_spaces y j_ini))
      in
      loop1 0 j_ini
  in
  loop 0

let select_both_start_with_person base ini_n ini_p =
  let find n x = string_start_with x n in
  let ini_n = aux_ini (Name.lower ini_n) in
  let ini_p = aux_ini (Name.lower ini_p) in
  Gwdb.Collection.fold begin fun list p ->
      let surnames = aux_ini (Name.lower (Gwdb.sou base (Gwdb.get_surname p))) in
      let first_names = aux_ini (Name.lower (Gwdb.sou base (Gwdb.get_first_name p))) in
      let start_surname =
        List.for_all
          (fun ini -> List.exists (fun name -> find name ini) surnames)
          ini_n
      in
      let start_firstname =
        List.for_all
          (fun ini -> List.exists (fun name -> find name ini) first_names)
          ini_p
      in
      if start_surname && start_firstname then (Gwdb.get_iper p :: list)
      else list
  end [] (Gwdb.persons base)

let select_start_with_person base get_field ini =
  let find n x = string_start_with x n in
  let ini = aux_ini (Name.lower ini) in
  Gwdb.Collection.fold begin fun list p ->
      let names = aux_ini (Name.lower (Gwdb.sou base (get_field p))) in
      let start_name =
        List.for_all
          (fun ini -> List.exists (fun name -> find name ini) names)
          ini
      in
      if start_name then (Gwdb.get_iper p :: list)
      else list
  end [] (Gwdb.persons base)

let matching_nameset base stop max_res istr name_f name first_letter =
  let rec aux n istr set =
    let s = Gwdb.sou base istr in
    let k = Geneweb.Util.name_key base s in
    if n < max_res && (stop && String.sub k 0 1 = first_letter || not stop) then
      let n, set =
        if string_incl_start_with (Name.lower name) (Name.lower k) then
          n + 1, Ext_string.Set.add s set
        else n, set
      in
      match Gwdb.spi_next name_f istr with
      | exception Not_found -> n, set
      | istr -> aux n istr set
    else n, set
  in
  aux 0 istr Ext_string.Set.empty

let matching_nameset' base stop max_res name_f name first_letter =
  match Gwdb.spi_first name_f first_letter with
  | exception Not_found -> 0, Ext_string.Set.empty
  | istr -> matching_nameset base stop max_res istr name_f name first_letter

let matching_nameset_of_input base stop uppercase name_f max_res name =
  let name' = Ext_string.tr '_' ' ' name in
  let first_letter = String.sub name' 0 1 in
  let first_letter =
    if uppercase then String.uppercase_ascii first_letter
    else String.lowercase_ascii first_letter
 in
 matching_nameset' base stop max_res name_f name first_letter

let select_start_with_auto_complete base mode max_res input =
  let name_f =
    match mode with
    | `lastname -> Gwdb.persons_of_surname base
    | `firstname -> Gwdb.persons_of_first_name base
  in
  (* Si la base est grosse > 100 000, on fait un vrai start_with. *)
  if Gwdb.nb_of_persons base > 100000 then
    begin
      let name =
        match mode with
        | `lastname -> Geneweb.Util.name_key base input
        | `firstname -> input
      in
      (* uppercase *)
      let nb_res, maj_set = matching_nameset_of_input base true true name_f max_res name in
      (* lowercase *)
      let _, min_set = matching_nameset_of_input base true false name_f (max_res - nb_res) name in
      Ext_string.Set.union maj_set min_set
    end
  else
    begin
      (* On commence à ? comme ça on fait MAJ et MIN. *)
      let starting_letter = "\000" in
      snd @@ matching_nameset' base false max_res name_f input starting_letter
    end

let select_start_with_auto_complete base mode max_res ini =
  let s = select_start_with_auto_complete base mode max_res ini in
  let l = Ext_string.Set.elements s in
  List.sort Utf8.alphabetic_order l

type dico = string array

let dico_fname ~assets ~lang ~data_type =
  Option.map (Filename.concat assets) @@ match data_type with
  | `town -> Some ("dico.town." ^ lang ^ ".bin~")
  | `area_code -> Some ("dico.area_code." ^ lang ^ ".bin~")
  | `county -> Some ("dico.county." ^ lang ^ ".bin~")
  | `region -> Some ("dico.region." ^ lang ^ ".bin~")
  | `country -> Some ("dico.country." ^ lang ^ ".bin~")
  | `subdivision -> None
  | `profession -> Some ("dico.profession." ^ lang ^ ".bin~")

let complete_with_dico assets conf nb max mode ini list =
  let split_country_code row =
    let rec aux acc l = match l with
      | [country_code] -> country_code, List.rev acc
      | x :: xs -> aux (x::acc) xs
      | _ -> assert false
    in
    aux [] row
  in
  let belongs_to_preferred_countries country_code =
    match conf.Geneweb.Config.preferred_countries with
      None -> true
    | Some codes -> List.mem country_code codes
  in
  let reduce_dico n mode ignored format list =
    let len = Array.length list in
    let rec loop n acc i =
      if i = len || n >= max
      then n, acc
      else
        let hd = Array.unsafe_get list i in
        let k =  Ext_string.tr '_' ' ' hd in
        let k =
          match mode with
          | `area_code | `country | `county | `region | `town ->
            Geneweb.Place.without_suburb k
          | `subdivision | `profession -> k
        in
        if string_start_with ini (Name.lower k) then begin
          let row = Api_csv.row_of_string hd in
          let hd_opt =
            match mode with
            | `profession -> Some (String.concat ", " row)
            | #Api_saisie_write_piqi.auto_complete_place_field ->
              let country_code, expl_hd = split_country_code row in
              if belongs_to_preferred_countries country_code then
                if format <> [] then
                  Some (String.concat ", " @@
                        List.filter_map begin function
                          | `town -> List.nth_opt expl_hd 0
                          | `area_code -> List.nth_opt expl_hd 1
                          | `county -> List.nth_opt expl_hd 2
                          | `region -> List.nth_opt expl_hd 3
                          | `country -> List.nth_opt expl_hd 4
                          | _ -> None
                        end
                          format)
                else Some (String.concat ", " expl_hd)
              else None
          in
          if Option.is_none hd_opt
          || List.mem (Option.get hd_opt) ignored
          then loop n acc (i + 1)
          else loop (n + 1) (Option.get hd_opt :: acc) (i + 1)
        end
        else loop n acc (i + 1)
    in loop n [] 0
  in
  let unmarshal_dico ~assets ~lang ~data_type =
    match dico_fname ~assets ~lang ~data_type with
    | Some fn -> Files.read_or_create_value fn (fun () : dico -> [||])
    | None -> [||]
  in
  match mode with
  | Some (#Api_saisie_write_piqi.auto_complete_place_field as mode)
       when nb < max ->
    let format =
      match List.assoc_opt "places_format" conf.Geneweb.Config.base_env with
      | None -> []
      | Some s ->
        List.map begin function
          | "Subdivision" -> `subdivision
          | "Town" -> `town
          | "Area code" -> `area_code
          | "County" -> `county
          | "Region" -> `region
          | "Country" -> `country
          | _ -> raise Not_found
          end
          (Api_csv.row_of_string s)
    in
    let _nb, dico =
      unmarshal_dico ~assets ~lang:conf.Geneweb.Config.lang ~data_type:mode
      |> reduce_dico nb mode list format
    in
    append list (List.sort Geneweb.Place.compare_places dico)
  | Some `profession when nb < max ->
     let dictionary =
       unmarshal_dico
         ~assets ~lang:conf.Geneweb.Config.lang ~data_type:`profession
     in
     dictionary
     |> reduce_dico nb `profession list []
     |> snd
     |> List.sort Utf8.alphabetic_order
     |> append list
  | None
  | Some (#Api_saisie_write_piqi.auto_complete_place_field | `profession) ->
    list

let get_all_data_from_db conf base data compare =
  let conf = { conf with Geneweb.Config.env = ("data", Mutil.encode data) :: conf.Geneweb.Config.env } in
  Geneweb.UpdateData.get_all_data conf base
  |> List.map (Gwdb.sou base)
  |> List.sort compare

let is_completion_suggestion ~mode ~place_mode ~ini ~candidate =
  match mode with
  | `source | `occupation ->
     string_start_with ini (Name.lower @@ Ext_string.tr '_' ' ' candidate)
  | `place as mode ->
    Api_saisie_autocomplete.is_valid_suggestion ~mode ~place_mode ~ini ~candidate:(Ext_string.tr '_' ' ' candidate)

let complete_with_db ~conf ~base ~max ~(mode : [`source | `occupation | `place]) ~place_mode ~ini =
  let list =
    let data, compare =
      match mode with
      | `source -> "src", Utf8.alphabetic_order
      | `occupation -> "occu", Utf8.alphabetic_order
      | `place -> "place", Geneweb.Place.compare_places
    in
    get_all_data_from_db conf base data compare
  in
  let rec reduce n acc = function
    | [] -> n, acc
    | candidate :: tl when n < max ->
      if is_completion_suggestion ~mode ~place_mode ~ini ~candidate
      then reduce (n + 1) (candidate :: acc) tl
      else reduce n acc tl
    | _candidate :: _tl -> n, acc
  in
  let n, res = reduce 0 [] list in
  n, List.rev res

let search_auto_complete ~assets ~conf ~base ~mode ~place_mode ~max ~ini =
  match (mode : Api_saisie_write_piqi.auto_complete_field) with

  | `place as mode ->
    let ini = Name.lower @@ Ext_string.tr '_' ' ' ini in
    let n, reduced_list =
      complete_with_db ~conf ~base ~max ~mode ~place_mode ~ini
    in
    complete_with_dico assets conf n max place_mode ini reduced_list

  | `source as mode ->
    let ini = Name.lower @@ Ext_string.tr '_' ' ' ini in
    snd (complete_with_db ~conf ~base ~max ~mode ~place_mode ~ini)

  | `firstname | `lastname as mode ->
    if Name.lower ini = "" then []
    else ( Gwdb.load_strings_array base
         ; select_start_with_auto_complete base mode max ini )

  | `occupation as mode ->
    let ini = Name.lower @@ Ext_string.tr '_' ' ' ini in
    let n, suggestions_from_db =
      complete_with_db ~conf ~base ~max ~mode ~place_mode ~ini
    in
    complete_with_dico
      assets conf n max (Some `profession) ini suggestions_from_db

let search_person_list base surname first_name =
  let _ = Gwdb.load_strings_array base in
  let (surname, first_name) =
    match (surname, first_name) with
    | (Some n, Some fn) ->
        ((if Name.lower n = "" then None else Some n),
         (if Name.lower fn = "" then None else Some fn))
    | (Some n, None) -> ((if n = "" then None else Some n), None)
    | (None, Some fn) -> (None, (if fn = "" then None else Some fn))
    | (None, None) -> (None, None)
  in
  match (surname, first_name) with
  | (Some n, Some fn) ->
      select_both_start_with_person base n fn
  | (Some n, None) ->
      select_start_with_person base Gwdb.get_surname n
  | (None, Some fn) ->
      select_start_with_person base Gwdb.get_first_name fn
  | (None, None) -> []
