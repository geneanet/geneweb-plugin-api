(* Dans un premier temps, ce module dupliques certaines  *)
(* fonctions déjà présentes, mais c'est pour qu'il reste *)
(* le plus indépendant possible des autres modules.      *)

(* Convert safe_string to string *)
let (!!) = Adef.as_string

(* ... utils ... *)

let p_getenvbin = Api_piqi_util.p_getenvbin
let get_params = Api_piqi_util.get_params

let is_empty_or_quest_name p =
  Gwdb.is_empty_string (Gwdb.get_surname p) || Gwdb.is_quest_string (Gwdb.get_surname p) ||
  Gwdb.is_empty_string (Gwdb.get_first_name p) || Gwdb.is_quest_string (Gwdb.get_first_name p)

let get_portrait conf base p = Geneweb.Image.get_portrait conf base p |> Option.map Geneweb.Image.src_to_string

(**/**)

let compute_sosa conf base person =
  Geneweb.Sosa_cache.get_sosa_person ~conf ~base ~person

(* Pour aller plus vite et ne pas tester l'existance de fichier    *)
(* plusieurs fois en fonction des extensions, on prend le problème *)
(* à l'envers et on charge tous les fichiers qui existe. Ensuite,  *)
(* on teste l'existence avec une Hashtbl.                          *)
let ht_img = Hashtbl.create 5003

let load_image_ht conf =
  let dir_img = Geneweb.GWPARAM.base_path ["images"] conf.Geneweb.Config.bname in
  let images =
    if Sys.file_exists dir_img then Sys.readdir dir_img
    else [||]
  in
  Array.iter
    (fun img ->
      if img = "old" then ()
      else
        try
          let name = Filename.chop_extension img in
          Hashtbl.add ht_img name img
        with _ -> ())
    images

(* BIENTOT DEPRECATED *)
let string_of_prec_dmy d =
  let s =
    if d.Date.month = 0 then string_of_int d.year
    else if d.day = 0 then string_of_int d.month ^ "/" ^ string_of_int d.year
    else string_of_int d.day ^ "/" ^ string_of_int d.month ^ "/" ^ string_of_int d.year
  in
  match d.prec with
   | Sure -> Mutil.nominative s
   | About -> "~" ^ s
   | Before -> "<" ^ s
   | After -> ">" ^ s
   | Maybe -> "?" ^ s
   | OrYear d2 -> s ^ "|" ^ string_of_int d2.year2
   | YearInt d2 -> s ^ ".." ^ string_of_int d2.year2

let string_of_date = function
    Date.Dgreg (d, _) -> string_of_prec_dmy d
  | Dtext t -> "(" ^ Utf8.normalize t ^ ")"

let title_to_piqi_title t =
  let t = Futil.map_title_strings Utf8.normalize t in
  let (title_type, name) =
    match t.t_name with
    | Tmain -> (`title_main, "")
    | Tname name -> (`title_name, name)
    | Tnone -> (`title_none, "")
  in
  let title = t.t_ident in
  let fief = t.t_place in
  let date_begin =
    match Date.od_of_cdate t.t_date_start with
    | Some d -> Some (string_of_date d)
    | None -> None
  in
  let date_end =
    match Date.od_of_cdate t.t_date_end with
    | Some d -> Some (string_of_date d)
    | None -> None
  in
  let nth = Some (Int32.of_int t.t_nth) in
  Api_piqi.Title.{ title_type
          ; name = if name = "" then None else Some name
          ; title = if title = "" then None else Some title
          ; fief = if fief = "" then None else Some fief
          ; date_begin
          ; date_end
          ; nth
          }


(**/**) (* Convertion d'une date. *)

module Date_converter
    (M : sig
       module Dmy : sig
         type t = { mutable day : int32
                  ; mutable month : int32
                  ; mutable year : int32
                  ; mutable delta : int32
                  }
       end
       module Date : sig
         type t = { mutable cal : [ `gregorian | `julian | `french | `hebrew ] option
                  ; mutable prec : [ `sure | `about | `maybe | `before | `after | `oryear | `yearint ] option
                  ; mutable dmy : Dmy.t option
                  ; mutable dmy2 : Dmy.t option
                  ; mutable text : string option
                  }
       end
     end) =
struct
  let piqi_date_of_date = function
    | Date.Dgreg (dmy, cal) ->
      let cal =
        match cal with
        | Dgregorian -> `gregorian
        | Djulian -> `julian
        | Dfrench -> `french
        | Dhebrew -> `hebrew
      in
      let (prec, dmy, dmy2) =
        let (d, m, y, delta) =
          (Int32.of_int dmy.day, Int32.of_int dmy.month,
           Int32.of_int dmy.year, Int32.of_int dmy.delta)
        in
        let dmy1 = {M.Dmy.day = d; month = m; year = y; delta = delta;} in
        let (prec, dmy2) =
          match dmy.prec with
          | Sure -> (`sure, None)
          | About -> (`about, None)
          | Maybe -> (`maybe, None)
          | Before -> (`before, None)
          | After -> (`after, None)
          | OrYear d2 ->
            let dmy2 =
              {
                M.Dmy.day = Int32.of_int 0;
                month = Int32.of_int 0;
                year = Int32.of_int d2.year2;
                delta = Int32.of_int 0;
              }
            in
            (`oryear, Some dmy2)
          | YearInt d2 ->
            let dmy2 =
              {
                M.Dmy.day = Int32.of_int 0;
                month = Int32.of_int 0;
                year = Int32.of_int d2.year2;
                delta = Int32.of_int 0;
              }
            in
            (`yearint, Some dmy2)
        in
        (prec, dmy1, dmy2)
      in
      {
        M.Date.cal = Some cal;
        prec = Some prec;
        dmy = Some dmy;
        dmy2 = dmy2;
        text = None;
      }
    | Dtext txt ->
      {
        M.Date.cal = None;
        prec = None;
        dmy = None;
        dmy2 = None;
        text = Some txt;
      }

  let calendar_of_piqi_calendar = function
    | `julian -> Date.Djulian
    | `french -> Dfrench
    | `hebrew -> Dhebrew
    | `gregorian  -> Dgregorian

  let date_of_piqi_date date =
    match date.M.Date.text with
    | Some txt -> Date.Dtext txt
    | _ ->
      let cal =
        Option.fold
          date.M.Date.cal ~some:calendar_of_piqi_calendar ~none:Date.Dgregorian
      in
      let prec =
        match date.M.Date.prec with
        | Some `about -> Date.About
        | Some `maybe -> Maybe
        | Some `before -> Before
        | Some `after -> After
        | Some `oryear ->
          (match date.M.Date.dmy2 with
           | Some dmy ->
             let y = Int32.to_int dmy.M.Dmy.year in
             let dmy2 = {Date.day2 = 0; month2 = 0; year2 = y; delta2 = 0} in
             OrYear dmy2
           | None -> OrYear {day2 = 0; month2 = 0; year2 = 0; delta2 = 0} (* erreur*))
        | Some `yearint ->
          (match date.M.Date.dmy2 with
           | Some dmy ->
             let y = Int32.to_int dmy.M.Dmy.year in
             let dmy2 = {Date.day2 = 0; month2 = 0; year2 = y; delta2 = 0} in
             YearInt dmy2
           | None -> YearInt {day2 = 0; month2 = 0; year2 = 0; delta2 = 0} (* erreur*))
        | _ -> Sure
      in
      let dmy =
        match date.M.Date.dmy with
        | Some dmy ->
          let day = Int32.to_int dmy.M.Dmy.day in
          let month = Int32.to_int dmy.M.Dmy.month in
          let year = Int32.to_int dmy.M.Dmy.year in
          let delta = Int32.to_int dmy.M.Dmy.delta in
          {Date.day = day; month = month; year = year; prec = prec; delta = delta}
        | None -> (* erreur*)
          {day = 0; month = 0; year = 0; prec = Sure; delta = 0}
      in
      Dgreg (dmy, cal)

end

include Date_converter (Api_piqi)


(* ********************************************************************* *)
(*  [Fonc] date_included : dmy -> dmy -> dmy -> bool                     *)
(** [Description] : d1 <= d <= d2
    [Args] :
      - d  : date
      - d1 : date min
      - d2 : date max
    [Retour] :
      - bool : renvoie d1 <= d <= d2.
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let date_included d d1 d2 =
  (* Fonction générique de test: y <= x <= z *)
  (* Le paramètre max permet de tester par   *)
  (* rapport au nombre max de jour ou mois.  *)
  let comp x y z max =
    if y <= z then (y <= x) && (x <= z)
    else if max > 0 then ((y <= x) && (x <= max)) || ((1 <= x) && (x <= z))
    else false
  in
  let (d, m, y) = (d.Date.day, d.month, d.year) in
  let { Date.day = d2 ; month = m2 ; year = y2 } = d2 in
  match d1 with
  | {Date.day = 0; month = 0; year = 0} -> false
  | {day = d1; month = 0; year = 0} ->
    d2 <> 0 && m2 = 0 && y2 = 0 && d > 0 && comp d d1 d2 31
  | {day = 0; month = m1; year = 0} ->
    m2 <> 0 && d2 = 0 && y2 = 0 && m > 0 && comp m m1 m2 12
  | {day = 0; month = 0; year = y1} ->
    y2 <> 0 && d2 = 0 && m2 = 0 && comp y y1 y2 0
  (* Impossible pour GeneWeb *)
  | {day = d1; month = m1; year = 0} ->
    d2 <> 0 && m2 <> 0 && y2 = 0
    && d > 0
    && m > 0
    && comp (m * 100 + d) (m1 * 100 + d1) (m2 * 100 + d2) (12 * 100 + 31)
  | {day = 0; month = m1; year = y1} ->
    m2 <> 0 && y2 <> 0 && d2 = 0
    && m > 0 && comp (y * 100 + m) (y1 * 100 + m1) (y2 * 100 + m2) 0
  (* Impossible pour GeneWeb *)
  | {day = d1; month = 0; year = y1} ->
    d2 <> 0 && y2 <> 0 && m2 = 0
    && d > 0 && y1 = y2 && comp d d1 d2 31
  | {day = d1; month = m1; year = y1} ->
    y2 <> 0 &&
    comp
      (y * 10000 + m * 100 + d)
      (y1 * 10000 + m1 * 100 + d1)
      (y2 * 10000 + m2 * 100 + d2)
      0

(**/**) (* Divers filtres possibles. *)

(** [get_visibility conf base p] is the visibility of [p] as defined by geneanet's rules.
    - [`visibility_public] if [p] is fully visible for a visitor
    - [`visibility_semi_public] if [p] is hidden but with some information such as names still visible
    - [`visibility_private] if [p] is fully hidden to a visitor
*)
let get_visibility conf base p =
  if Geneweb.Util.is_fully_visible_to_visitors conf base p then `visibility_public
  else if conf.hide_private_names || Gwdb.get_access p = Private then `visibility_private
  else `visibility_semi_public


(* ********************************************************************* *)
(*  [Fonc] is_sosa : (person -> Sosa.t) -> person -> bool            *)
(** [Description] : Test si la personne est un sosa.
    [Retour] : bool
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let is_sosa compute_sosa p =
  Sosa.gt (compute_sosa p) Sosa.zero


(* ********************************************************************* *)
(*  [Fonc] is_recent : config -> person -> bool                          *)
(** [Description] : Test si la personne est un contemporain.
    [Args] :
      - conf : configuration de la base
      - p    : person
    [Retour] : bool
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let is_recent conf p =
  let tmp_conf =
    {(conf) with Geneweb.Config.private_years = max 85 conf.Geneweb.Config.private_years;
      (* !!! Si on n'a pas de dates, on considère qu'on est contemporain.
         (Mantis 1327) *)
      public_if_no_date = false}
  in
  not (Geneweb.Util.is_old_person tmp_conf (Gwdb.gen_person_of_person p))


(* ********************************************************************* *)
(*  [Fonc] check_sex : person -> Def.sex -> bool                         *)
(** [Description] : Test si la personne est du même sexe que sex.
    [Args] :
      - conf : configuration de la base
      - p    : person
      - sex  : sexe que l'on cherche
    [Retour] : bool
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let check_sex p sex = Gwdb.get_sex p = sex


(* ********************************************************************* *)
(*  [Fonc] is_date_included : bool -> date -> date -> date -> bool       *)
(** [Description] : Test si d1 <= d <= d2.
    [Args] :
      - prec   : booléen pour savoir si l'on veut tester une date précise
                 (par exemple Octobre 1800 n'est pas une date précise)
      - d      : date que l'on cherche
      - d1, d2 : interval de date
    [Retour] : bool
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let is_date_included prec d d1 d2 =
  match d with
  | Some (Date.Dgreg (d, _)) ->
      ((prec && d.prec = Sure) || not prec) && date_included d d1 d2
  | _ -> false


(* ********************************************************************* *)
(*  [Fonc] apply_filters_p : config -> filters -> person -> bool *)
(** [Description] : Test en fonction des filtres défini si la personne
                    répond aux critères (true) ou pas (false).
    [Args] :
      - conf    : configuration de la base
      - filters : filtres demandés
      - p       : person
    [Retour] : bool
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let apply_filters_p conf filters compute_sosa p =
  let filter = true in
  let filter =
    if filter then
      match filters.Api_def.filter_sex with
      | Some sex -> check_sex p sex
      | None -> filter
    else filter
  in
  let filter =
    if filter && filters.only_sosa then is_sosa compute_sosa p
    else filter
  in
  let filter =
    if filter && filters.only_recent then is_recent conf p
    else filter
  in
  let filter =
    if filter then
      match filters.date_birth with
      | Some (date_begin, date_end, prec) ->
          is_date_included
            prec (Date.od_of_cdate (Gwdb.get_birth p)) date_begin date_end
      | None -> filter
    else filter
  in
  if filter then
    match filters.date_death with
    | Some (date_begin, date_end, prec) ->
      let death =
        match Gwdb.get_death p with
        | Death (_, cd) -> Some (Date.date_of_cdate cd)
        | _ -> None
      in
      is_date_included prec death date_begin date_end
    | None -> filter
  else filter


(**/**) (* Fonctions IO *)

module Filter = Api_piqi_util.Filter (Api_piqi) (Api_piqi_ext)

module ReferencePerson = Api_piqi_util.ReferencePerson (Api_piqi)

let person_to_reference_person = ReferencePerson.person_to_reference_person

let empty_reference_person = ReferencePerson.empty_reference_person

let get_filters = Filter.get_filters

let print_result = Api_piqi_util.print_result

let date_to_opt_string d =
  match Date.od_of_cdate d with
  | Some d -> Some (string_of_date d)
  | _ -> None


let person_to_warning_person base p =
  let iper = Gwdb.string_of_iper (Gwdb.get_iper p) in
  let lastname = Utf8.normalize (Gwdb.sou base (Gwdb.get_surname p)) in
  let firstname = Utf8.normalize (Gwdb.sou base (Gwdb.get_first_name p)) in
  let birth_date = date_to_opt_string @@ Gwdb.get_birth p in
  let death_date =
    match Gwdb.get_death p with
    | Death (_, d) -> date_to_opt_string d
    | _ -> None
  in
  let oc = Int32.of_int (Gwdb.get_occ p) in
  let n = Name.lower lastname in
  let p = Name.lower firstname in
  { Api_piqi.Warning_person.n
  ; p
  ; oc
  ; firstname
  ; lastname
  ; birth_date
  ; death_date
  ; iper
  }

(**/**) (* Fonctions de transformation person <=> piqi person *)

let piqi_ref_person_to_person base ref_person =
  let sn = ref_person.Api_piqi.Reference_person.n in
  let fn = ref_person.Api_piqi.Reference_person.p in
  let occ = ref_person.Api_piqi.Reference_person.oc in
  match Gwdb.person_of_key base fn sn (Int32.to_int occ) with
  | Some ip -> Some (Gwdb.poi base ip)
  | None -> None


(* ********************************************************************* *)
(*  [Fonc] empty_piqi_person_light : Reference_person -> Person          *)
(** [Description] : Retourne à partir d'une Reference_person, une Person
                    dont tous les champs sont "vide" sauf (n, p, oc).
    [Args] :
      - ref_person : Reference_person
    [Retour] :
      - Person : Retourne une personne "vide".
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let empty_piqi_person_light conf ref_person =
  let sn = ref_person.Api_piqi.Reference_person.n in
  let fn = ref_person.Api_piqi.Reference_person.p in
  let occ = ref_person.Api_piqi.Reference_person.oc in
  {
    Api_piqi.Person.sosa = "0";
    n = sn;
    p = fn;
    oc = occ;
    index = Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
    sex = `unknown;
    lastname = "";
    firstname = "";
    public_name = None;
    image = "";
    birth_date = "";
    birth_place = "";
    baptism_date = "";
    baptism_place = "";
    death_date = "";
    death_place = "";
    death_type = `not_dead;
    burial_date = "";
    burial_place = "";
    spouses = [];
    ascend = false;
    descend = false;
    visible_for_visitors = `visibility_private;
    baseprefix = conf.Geneweb.Config.command;
    is_contemporary = true;
    name_is_hidden = false;
    name_is_restricted = false;
  }


(* ********************************************************************* *)
(*  [Fonc] empty_piqi_person_full : Reference_person -> Person           *)
(** [Description] : Retourne à partir d'une Reference_person, une Person
                    dont tous les champs sont "vide" sauf (n, p, oc).
    [Args] :
      - ref_person : Reference_person
    [Retour] :
      - Person : Retourne une personne "vide".
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let empty_piqi_person_full conf ref_person =
  let sn = ref_person.Api_piqi.Reference_person.n in
  let fn = ref_person.Api_piqi.Reference_person.p in
  let occ = ref_person.Api_piqi.Reference_person.oc in
  {
    Api_piqi.Full_person.sosa = "0";
    n = sn;
    p = fn;
    oc = occ;
    index = Int32.of_string @@ Gwdb.string_of_iper Gwdb.dummy_iper;
    sex = `unknown;
    lastname = "";
    firstname = "";
    public_name = None;
    aliases = [];
    qualifiers = [];
    firstname_aliases = [];
    surname_aliases = [];
    image = None;
    birth_date = None;
    birth_place = None;
    birth_src = None;
    baptism_date = None;
    baptism_place = None;
    baptism_src = None;
    death_date = None;
    death_place = None;
    death_src = None;
    death_type = `not_dead;
    burial_date = None;
    burial_place = None;
    burial_src = None;
    occupation = None;
    psources = None;
    titles = [];
    related = [];
    rparents = [];
    visible_for_visitors = `visibility_private;
    parents = None;
    families = [];
    baseprefix = conf.Geneweb.Config.command;
    is_contemporary = true;
    name_is_hidden = false;
    name_is_restricted = false;
  }


let empty_piqi_person conf ref_person =
  if p_getenvbin conf.Geneweb.Config.env "full_infos" = Some "1"
  then Api_def.PFull (empty_piqi_person_full conf ref_person)
  else Api_def.PLight (empty_piqi_person_light conf ref_person)


(* ************************************************************************** *)
(*  [Fonc] spouse_to_piqi_spouse :
             config -> base -> person -> family -> bool ->
             (person -> Sosa.t) -> Perso                                      *)
(** [Description] : Retourne à partir d'une person (gwdb) une Spouse (piqi)
                    dont tous les champs sont complétés.
                    Les tests de droits d'accès sont fait dans cette fonction.
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - p         : person
      - fam       : family
      - base_loop : booléen pour savoir s'il y a une boucle dans la base.
      - compute_sosa : appel de Geneweb.Sosa_cache.get_sosa_person
    [Retour] :
      - Person : Retourne une personne dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let spouse_to_piqi_spouse conf base p fam compute_sosa =
  let gen_p = Geneweb.Util.string_gen_person base (Gwdb.gen_person_of_person p) in
  let gen_p = Futil.map_person_ps Fun.id (fun ?format:_ -> Utf8.normalize) gen_p in
  let p_auth = Geneweb.Person.is_visible conf base p in
  let ifath = Gwdb.get_father fam in
  let imoth = Gwdb.get_mother fam in
  let m_auth =
    Geneweb.Person.is_visible conf base (Geneweb.Util.pget conf base ifath) &&
    Geneweb.Person.is_visible conf base (Geneweb.Util.pget conf base imoth)
  in
  let sosa_p = Sosa.to_string (compute_sosa p) in
  let sex =
    match gen_p.sex with
    | Male -> `male
    | Female -> `female
    | Neuter -> `unknown
  in
  let surname =
    if not p_auth && (Geneweb.Util.is_hide_names conf p) then ""
    else gen_p.surname
  in
  let first_name =
    if not p_auth && (Geneweb.Util.is_hide_names conf p) then ""
    else gen_p.first_name
  in
  let sn = Name.lower surname in
  let fn = Name.lower first_name in
  let occ = Int32.of_int (Gwdb.get_occ p) in
  let publicname = if gen_p.public_name = "" then None else Some gen_p.public_name in
  let image =
    match Geneweb.Image.get_portrait conf base p with
    | Some src ->
        if gen_p.image <> "" then gen_p.image else Geneweb.Image.src_to_string src
    | None -> ""
  in
  let birth =
    match Date.od_of_cdate gen_p.birth with
    | Some d when p_auth -> string_of_date d
    | _ -> ""
  in
  let birth_place =
    if p_auth then gen_p.birth_place
    else ""
  in
  let baptism =
    match Date.od_of_cdate gen_p.baptism with
    | Some d when p_auth -> string_of_date d
    | _ -> ""
  in
  let baptism_place =
    if p_auth then gen_p.baptism_place
    else ""
  in
  let (death_type, death) =
    if p_auth then
      match gen_p.death with
      | NotDead -> (`not_dead, "")
      | Death (_, cd) ->
          let d = Date.date_of_cdate cd in
          (`dead, string_of_date d)
      | DeadYoung -> (`dead_young, "")
      | DeadDontKnowWhen -> (`dead_dont_know_when, "")
      | DontKnowIfDead -> (`dont_know_if_dead, "")
      | OfCourseDead -> (`of_course_dead, "")
    else
      (`not_dead, "")
  in
  let death_place =
    if p_auth then gen_p.death_place
    else ""
  in
  let burial =
    match gen_p.burial with
    | Buried cod | Cremated cod ->
        (match Date.od_of_cdate cod with
        | Some d when p_auth -> string_of_date d
        | _ -> "")
    | _ -> ""
  in
  let burial_place =
    if p_auth then gen_p.death_place
    else ""
  in
  let marriage_date =
    match Date.od_of_cdate (Gwdb.get_marriage fam) with
    | Some d when m_auth -> string_of_date d
    | _ -> ""
  in
  let marriage_place =
    if m_auth then Utf8.normalize (Gwdb.sou base (Gwdb.get_marriage_place fam))
    else ""
  in
  let divorce_type =
    if m_auth then
      match Gwdb.get_divorce fam with
      | NotDivorced -> `not_divorced
      | Divorced _ -> `divorced
      | Separated -> `separated
    else `not_divorced
  in
  {
    Api_piqi.Spouse.sosa = sosa_p;
    n = sn;
    p = fn;
    oc = occ;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    public_name = publicname;
    image = image;
    birth_date = birth;
    birth_place = birth_place;
    baptism_date = baptism;
    baptism_place = baptism_place;
    death_date = death;
    death_place = death_place;
    death_type = death_type;
    burial_date = burial;
    burial_place = burial_place;
    marriage_date = marriage_date;
    marriage_place = marriage_place;
    divorce_type = divorce_type;
    visible_for_visitors = get_visibility conf base p;
    index = Int32.of_string (Gwdb.string_of_iper gen_p.key_index);
  }


(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_person_light :
             config -> base -> person -> bool -> (person -> Sosa.t) -> Person *)
(** [Description] : Retourne à partir d'une person (gwdb) une Person (piqi)
                    (piqi) dont tous les champs sont complétés.
                    Les tests de droits d'accès sont fait dans cette fonction.
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - p         : person
      - base_loop : booléen pour savoir s'il y a une boucle dans la base.
      - compute_sosa : appel de Geneweb.Sosa_cache.get_sosa_person
    [Retour] :
      - Person : Retourne une personne dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_person_light conf base p compute_sosa =
  let gen_p = Geneweb.Util.string_gen_person base (Gwdb.gen_person_of_person p) in
  let gen_p = Futil.map_person_ps Fun.id (fun ?format:_ -> Utf8.normalize) gen_p in
  let p_auth = Geneweb.Person.is_visible conf base p in
  let sosa_p = Sosa.to_string (compute_sosa p) in
  let sex =
    match gen_p.sex with
    | Male -> `male
    | Female -> `female
    | Neuter -> `unknown
  in
  let surname =
    if not p_auth && (Geneweb.Util.is_hide_names conf p) then ""
    else gen_p.surname
  in
  let first_name =
    if not p_auth && (Geneweb.Util.is_hide_names conf p) then ""
    else gen_p.first_name
  in
  let sn = Name.lower surname in
  let fn = Name.lower first_name in
  let occ = Int32.of_int (Gwdb.get_occ p) in
  let publicname = if gen_p.public_name = "" then None else Some gen_p.public_name in
  let image =
    match Geneweb.Image.get_portrait conf base p with
    | Some src ->
        if gen_p.image <> "" then gen_p.image else Geneweb.Image.src_to_string src
    | None -> ""
  in
  let birth =
    match Date.od_of_cdate gen_p.birth with
    | Some d when p_auth -> string_of_date d
    | _ -> ""
  in
  let birth_place =
    if p_auth then gen_p.birth_place
    else ""
  in
  let baptism =
    match Date.od_of_cdate gen_p.baptism with
    | Some d when p_auth -> string_of_date d
    | _ -> ""
  in
  let baptism_place =
    if p_auth then gen_p.baptism_place
    else ""
  in
  let (death_type, death) =
    if p_auth then
      match gen_p.death with
      | NotDead -> (`not_dead, "")
      | Death (_, cd) ->
          let d = Date.date_of_cdate cd in
          (`dead, string_of_date d)
      | DeadYoung -> (`dead_young, "")
      | DeadDontKnowWhen -> (`dead_dont_know_when, "")
      | DontKnowIfDead -> (`dont_know_if_dead, "")
      | OfCourseDead -> (`of_course_dead, "")
    else
      (`not_dead, "")
  in
  let death_place =
    if p_auth then Utf8.normalize (Gwdb.sou base (Gwdb.get_death_place p))
    else ""
  in
  let burial =
    match gen_p.burial with
    | Buried cod | Cremated cod ->
        (match Date.od_of_cdate cod with
        | Some d when p_auth -> string_of_date d
        | _ -> "")
    | _ -> ""
  in
  let burial_place =
    if p_auth then gen_p.burial_place
    else ""
  in
  let faml = Array.to_list (Gwdb.get_family p) in
  let sl =
    List.map
      (fun ifam ->
        let fam = Gwdb.foi base ifam in
        let c = Gutil.spouse (Gwdb.get_iper p) fam in
        (Geneweb.Util.pget conf base c, fam) )
      faml
  in
  let sl =
    List.map
      (fun (p, fam) ->
        spouse_to_piqi_spouse conf base p fam compute_sosa)
      sl
  in
  let ascend = Gwdb.get_parents p <> None in
  let descend =
    List.exists
      (fun c -> Array.length (Gwdb.get_children c) > 0)
      (List.map (Gwdb.foi base) faml)
  in
  let baseprefix = conf.command in
  let index = Int32.of_string @@ Gwdb.string_of_iper gen_p.key_index in
  {
    Api_piqi.Person.sosa = sosa_p;
    n = sn;
    p = fn;
    oc = occ;
    index;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    public_name = publicname;
    image = image;
    birth_date = birth;
    birth_place = birth_place;
    baptism_date = baptism;
    baptism_place = baptism_place;
    death_date = death;
    death_place = death_place;
    death_type = death_type;
    burial_date = burial;
    burial_place = burial_place;
    spouses = sl;
    ascend = ascend;
    descend = descend;
    visible_for_visitors = get_visibility conf base p;
    baseprefix = baseprefix;
    is_contemporary = Geneweb.Person.is_contemporary conf base p;
    name_is_hidden = Geneweb.NameDisplay.is_hidden conf base p;
    name_is_restricted = Geneweb.NameDisplay.is_restricted conf base p;
  }


(* ************************************************************************** *)
(*  [Fonc] pers_to_piqi_person_full :
             config -> base -> person -> bool ->
               (person -> Sosa.t) -> FullPerson              *)
(** [Description] : Retourne à partir d'une person (gwdb) une Person (piqi)
                    (piqi) dont tous les champs sont complétés.
                    Les tests de droits d'accès sont fait dans cette fonction.
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - p         : person
      - base_loop : booléen pour savoir s'il y a une boucle dans la base.
      - compute_sosa : appel de Geneweb.Sosa_cache.get_sosa_person
    [Retour] :
      - Person : Retourne une personne dont tous les champs sont complétés.
    [Rem] : Non exporté en clair hors de ce module.                           *)
(* ************************************************************************** *)
let pers_to_piqi_person_full conf base p compute_sosa =
  let gen_p = Geneweb.Util.string_gen_person base (Gwdb.gen_person_of_person p) in
  let gen_p = Futil.map_person_ps Fun.id (fun ?format:_ -> Utf8.normalize) gen_p in
  let p_auth = Geneweb.Person.is_visible conf base p in
  let sosa_p = Sosa.to_string (compute_sosa p) in
  let sex =
    match gen_p.sex with
    | Male -> `male
    | Female -> `female
    | Neuter -> `unknown
  in
  let surname =
    if not p_auth && (Geneweb.Util.is_hide_names conf p) then ""
    else gen_p.surname
  in
  let first_name =
    if not p_auth && (Geneweb.Util.is_hide_names conf p) then ""
    else gen_p.first_name
  in
  let sn = Name.lower surname in
  let fn = Name.lower first_name in
  let occ = Int32.of_int (Gwdb.get_occ p) in
  let index = Int32.of_string @@ Gwdb.string_of_iper gen_p.key_index in
  let publicname = if gen_p.public_name = "" then None else Some gen_p.public_name in
  let aliases = gen_p.aliases in
  let qualifiers =
    if not p_auth && (Geneweb.Util.is_hide_names conf p) then []
    else gen_p.qualifiers
  in
  let firstname_aliases = gen_p.first_names_aliases in
  let surname_aliases = gen_p.surnames_aliases in
  let image =
    match Geneweb.Image.get_portrait conf base p with
    | Some src ->
        if gen_p.image <> "" then gen_p.image else Geneweb.Image.src_to_string src
    | None -> ""
  in
  let birth =
    match Date.od_of_cdate gen_p.birth with
    | Some d when p_auth -> Some (string_of_date d)
    | _ -> None
  in
  let birth_place =
    if p_auth then Some gen_p.birth_place
    else None
  in
  let birth_src =
    if p_auth then Some gen_p.birth_src
    else None
  in
  let baptism =
    match Date.od_of_cdate gen_p.baptism with
    | Some d when p_auth -> Some (string_of_date d)
    | _ -> None
  in
  let baptism_place =
    if p_auth then Some gen_p.baptism_place
    else None
  in
  let baptism_src =
    if p_auth then Some gen_p.baptism_src
    else None
  in
  let (death_type, death) =
    if p_auth then
      match gen_p.death with
      | NotDead -> (`not_dead, None)
      | Death (_, cd) ->
          let d = Date.date_of_cdate cd in
          (`dead, Some (string_of_date d))
      | DeadYoung -> (`dead_young, None)
      | DeadDontKnowWhen -> (`dead_dont_know_when, None)
      | DontKnowIfDead -> (`dont_know_if_dead, None)
      | OfCourseDead -> (`of_course_dead, None)
    else
      (`not_dead, None)
  in
  let death_place =
    if p_auth then Some gen_p.death_place
    else None
  in
  let death_src =
    if p_auth then Some gen_p.death_src
    else None
  in
  let burial =
    match Gwdb.get_burial p with
    | Buried cod | Cremated cod ->
        (match Date.od_of_cdate cod with
        | Some d when p_auth -> Some (string_of_date d)
        | _ -> None)
    | _ -> None
  in
  let burial_place =
    if p_auth then Some gen_p.burial_place
    else None
  in
  let burial_src =
    if p_auth then Some gen_p.burial_src
    else None
  in
  let titles = List.map title_to_piqi_title gen_p.titles in
  let occupation =
    if p_auth then Some gen_p.occupation
    else None
  in
  let psources =
    if p_auth then Some gen_p.psources
    else None
  in
  let related = List.map (fun x -> Int32.of_string @@ Gwdb.string_of_iper x) (Gwdb.get_related p) in
  let rparents =
    List.map
      (fun rp ->
        let father =
          match rp.Def.r_fath with
          | Some ip -> Some (Int32.of_string @@ Gwdb.string_of_iper ip)
          | None -> None
        in
        let mother =
          match rp.r_moth with
          | Some ip -> Some (Int32.of_string @@ Gwdb.string_of_iper ip)
          | None -> None
        in
        let source = rp.r_sources in
        let rpt_type =
          match rp.r_type with
          | Adoption -> `rpt_adoption
          | Recognition -> `rpt_recognition
          | CandidateParent -> `rpt_candidate_parent
          | GodParent -> `rpt_god_parent
          | FosterParent -> `rpt_foster_parent
        in
        Api_piqi.Relation_parent.({
          father = father;
          mother = mother;
          source = if source = "" then None else Some source;
          rpt_type = rpt_type;
        }))
      gen_p.rparents
  in
  let families =
    Ext_array.to_list_map (fun x -> Int32.of_string @@ Gwdb.string_of_ifam x) (Gwdb.get_family p)
  in
  let parents =
    match Gwdb.get_parents p with
     | Some ifam -> Some (Int32.of_string (Gwdb.string_of_ifam ifam))
     | None -> None
  in
  let baseprefix = conf.command
  in
  {
    Api_piqi.Full_person.sosa = sosa_p;
    n = sn;
    p = fn;
    oc = occ;
    index = index;
    sex = sex;
    lastname = surname;
    firstname = first_name;
    public_name = publicname;
    aliases = aliases;
    qualifiers = qualifiers;
    firstname_aliases = firstname_aliases;
    surname_aliases = surname_aliases;
    image = if image = "" then None else Some image;
    birth_date = birth;
    birth_place = birth_place;
    birth_src = birth_src;
    baptism_date = baptism;
    baptism_place = baptism_place;
    baptism_src = baptism_src;
    death_date = death;
    death_place = death_place;
    death_src = death_src;
    death_type = death_type;
    burial_date = burial;
    burial_place = burial_place;
    burial_src = burial_src;
    occupation = occupation;
    psources = psources;
    titles = titles;
    related = related;
    rparents = rparents;
    visible_for_visitors = get_visibility conf base p;
    parents = parents;
    families = families;
    baseprefix = baseprefix;
    is_contemporary = Geneweb.Person.is_contemporary conf base p;
    name_is_hidden = Geneweb.NameDisplay.is_hidden conf base p;
    name_is_restricted = Geneweb.NameDisplay.is_restricted conf base p;
  }


let pers_to_piqi_person conf base p compute_sosa =
  if p_getenvbin conf.Geneweb.Config.env "full_infos" = Some "1"
  then Api_def.PFull (pers_to_piqi_person_full conf base p compute_sosa)
  else Api_def.PLight (pers_to_piqi_person_light conf base p compute_sosa)


(* ********************************************************************* *)
(*  [Fonc] fam_to_piqi_family : config -> base -> ifam -> Full_family    *)
(** [Description] :
    [Args] :
      - conf  : configuration de la base
      - base  : base de donnée
      - ifam  : ifam
    [Retour] :
      -
    [Rem] : Non exporté en clair hors de ce module.                      *)
(* ********************************************************************* *)
let fam_to_piqi_family conf base ifam =
  let fam = Gwdb.foi base ifam in
  let gen_f = Geneweb.Util.string_gen_family base (Gwdb.gen_family_of_family fam) in
  let gen_f = Futil.map_family_ps Fun.id Fun.id (fun ?format:_ -> Utf8.normalize) gen_f in
  let ifath = Gwdb.get_father fam in
  let imoth = Gwdb.get_mother fam in
  let m_auth =
    Geneweb.Person.is_visible conf base (Geneweb.Util.pget conf base ifath) &&
    Geneweb.Person.is_visible conf base (Geneweb.Util.pget conf base imoth)
  in
  let index = Int32.of_string @@ Gwdb.string_of_ifam ifam in
  let fsources =
    if m_auth then Some gen_f.fsources
    else None
  in
  let marriage =
    match Date.od_of_cdate gen_f.marriage with
    | Some d when m_auth -> Some (string_of_date d)
    | _ -> None
  in
  let marriage_place =
    if m_auth then Some gen_f.marriage_place
    else None
  in
  let marriage_src =
    if m_auth then Some gen_f.marriage_src
    else None
  in
  let marriage_type =
    match gen_f.relation with
    | Married -> `married
    | NotMarried -> `not_married
    | Engaged -> `engaged
    | NoSexesCheckNotMarried -> `no_sexes_check_not_married
    | NoMention -> `no_mention
    | NoSexesCheckMarried -> `no_sexes_check_married
    | MarriageBann -> `marriage_bann
    | MarriageContract -> `marriage_contract
    | MarriageLicense -> `marriage_license
    | Pacs -> `pacs
    | Residence -> `residence
  in
  let (divorce_type, divorce_date) =
    match gen_f.divorce with
    | NotDivorced -> (`not_divorced, None)
    | Divorced cod ->
        (match Date.od_of_cdate cod with
         | Some d when m_auth -> (`divorced, Some (string_of_date d))
         | _ -> (`divorced, None))
    | Separated -> (`separated, None)
  in
  let witnesses =
    List.map (fun x -> Int32.of_string @@ Gwdb.string_of_iper x) (Array.to_list gen_f.witnesses)
  in
  let father = Int32.of_string @@ Gwdb.string_of_iper ifath in
  let mother = Int32.of_string @@ Gwdb.string_of_iper imoth in
  let children =
    List.map (fun x -> Int32.of_string @@ Gwdb.string_of_iper x) (Array.to_list (Gwdb.get_children fam))
  in
  {
    Api_piqi.Full_family.fsources = fsources;
    marriage_date = marriage;
    marriage_place = marriage_place;
    marriage_src = marriage_src;
    marriage_type = marriage_type;
    divorce_type = divorce_type;
    divorce_date = divorce_date;
    witnesses = witnesses;
    father = father;
    mother = mother;
    children = children;
    index = index;
  }


(**/**) (* Fonctions de conversion *)

let data_person p =
  match p with
  | Api_def.PLight p -> Encoders.Api.encode_person p
  | Api_def.PFull p -> Encoders.Api.encode_full_person p

let person_map conf base l compute_sosa =
  if p_getenvbin conf.Geneweb.Config.env "full_infos" = Some "1" then
    Api_def.PFull
      (List.map
         (fun p -> pers_to_piqi_person_full conf base p compute_sosa)
         l)
  else
    Api_def.PLight
      (List.map
         (fun p -> pers_to_piqi_person_light conf base p compute_sosa)
         l)

let conv_data_list_person conf base filters l =
  let len = List.length l in
  if filters.Api_def.nb_results then
    let len = Api_piqi.Internal_int32.({value = Int32.of_int len}) in
    fun fmt -> Mext.gen_internal_int32 len (match fmt with Protoc_fmt.Protobuf -> `pb | Protoc_fmt.Json -> `json)
  else
    let compute_sosa = compute_sosa conf base in
    let l = person_map conf base l compute_sosa in
    match l with
    | Api_def.PLight pl ->
        let list = Api_piqi.List_persons.({list_persons = pl}) in
      fun fmt -> Encoders.Api.encode_list_persons list fmt
    | Api_def.PFull pl ->
        let list = Api_piqi.List_full_persons.({persons = pl}) in
      fun fmt -> Encoders.Api.encode_list_full_persons list fmt

let data_list_person_option conf base filters l =
  let len = List.length l in
  let compute_sosa = compute_sosa conf base in
  if filters.Api_def.nb_results then
    let len = Api_piqi.Internal_int32.({value = Int32.of_int len}) in
    fun fmt -> Mext.gen_internal_int32 len (Protoc_fmt.to_piqi fmt)
  else
    let l =
      if p_getenvbin conf.env "full_infos" = Some "1" then
        Api_def.PFull
          (List.map
            (fun p ->
              match p with
              | Api_def.PFull p ->
                  if apply_filters_p conf filters compute_sosa p then
                    pers_to_piqi_person_full conf base p compute_sosa
                  else
                    let ref_p = person_to_reference_person base p in
                    empty_piqi_person_full conf ref_p
              | Api_def.PLight ref_p -> empty_piqi_person_full conf ref_p)
            l)
      else
        Api_def.PLight
          (List.map
            (fun p ->
              match p with
              | Api_def.PFull p ->
                  if apply_filters_p conf filters compute_sosa p then
                    pers_to_piqi_person_light conf base p compute_sosa
                  else
                    let ref_p = person_to_reference_person base p in
                    empty_piqi_person_light conf ref_p
              | Api_def.PLight ref_p -> empty_piqi_person_light conf ref_p)
            l)
    in
    match l with
    | Api_def.PLight pl ->
        let list = Api_piqi.List_persons.({list_persons = pl}) in
        Encoders.Api.encode_list_persons list
    | Api_def.PFull pl ->
        let list = Api_piqi.List_full_persons.({persons = pl}) in
        Encoders.Api.encode_list_full_persons list

let person_node_map conf base l =
  let compute_sosa = compute_sosa conf base in
  if p_getenvbin conf.env "full_infos" = Some "1" then
    Api_def.PFull
      (List.rev_map
         (fun p ->
           let id = Int64.of_string @@ Gwdb.string_of_iper (Gwdb.get_iper p) in
           let p =
             pers_to_piqi_person_full conf base p compute_sosa
           in
           Api_piqi.Full_node.({
             id = id;
             person = p;
           }))
         l)
  else
    Api_def.PLight
      (List.rev_map
         (fun p ->
           let id = Int64.of_string @@ Gwdb.string_of_iper (Gwdb.get_iper p) in
           let p =
             pers_to_piqi_person_light conf base p compute_sosa
           in
           Api_piqi.Node.({
             id = id;
             person = p;
           }))
         l)

let chop_base_prefix base_prefix =
  let len = String.length base_prefix in
  if len > 2 &&
     (base_prefix.[len-1] = 'w' || base_prefix.[len-1] = 'f') &&
     base_prefix.[len-2] = '_'
  then
    String.sub base_prefix 0 (len - 2)
  else base_prefix


let print_error = Api_piqi_util.print_error

let witness_kind_of_piqi = function
  | `witness                  -> Def.Witness
  | `witness_godparent        -> Witness_GodParent
  | `witness_civilofficer     -> Witness_CivilOfficer
  | `witness_religiousofficer -> Witness_ReligiousOfficer
  | `witness_informant        -> Witness_Informant
  | `witness_attending        -> Witness_Attending
  | `witness_mentioned        -> Witness_Mentioned
  | `witness_other            -> Witness_Other

let piqi_of_witness_kind = function
  | Def.Witness                  -> `witness
  | Witness_GodParent        -> `witness_godparent
  | Witness_CivilOfficer     -> `witness_civilofficer
  | Witness_ReligiousOfficer -> `witness_religiousofficer
  | Witness_Informant        -> `witness_informant
  | Witness_Attending        -> `witness_attending
  | Witness_Mentioned        -> `witness_mentioned
  | Witness_Other            -> `witness_other

let translate_witness conf witness_kind =
  Geneweb.Util.string_of_witness_kind conf Def.Neuter witness_kind

let witness_kinds = [
    Def.Witness;
    Witness_GodParent;
    Witness_CivilOfficer;
    Witness_ReligiousOfficer;
    Witness_Informant;
    Witness_Attending;
    Witness_Mentioned;
    Witness_Other
  ]

let piqi_death_type_of_death = function
  | Def.NotDead -> `not_dead
  | DontKnowIfDead -> `dont_know_if_dead
  | OfCourseDead -> `of_course_dead
  | Death _ | DeadDontKnowWhen -> `dead
  | DeadYoung -> `dead_young

let opt_of_string = function
  | "" -> None
  | s -> Some s

let set_sosa_ref conf iper =
  {conf with Geneweb.Config.env = ("iz", Adef.encoded (Gwdb.string_of_iper iper)) :: conf.Geneweb.Config.env}

module Page = struct
  type t = {number : int; element_count : int}

  let make ~number ~element_count = {number; element_count}

  let first ~element_count = make ~number:1 ~element_count

  module Piqi = struct
    let from_page {Api_saisie_read_piqi.Page.number; element_count} =
      {number = Int32.to_int number; element_count = Int32.to_int element_count}
  end
end

module Paginated_data = struct
  type 'element t =
    {elements : 'element list; page_number : int; total_count : int}

  let all elements =
    {elements; page_number = 1; total_count = List.length elements}

  let count_elements_before {Page.number; element_count} =
    pred number * element_count

  let extract page all_elements =
    let elements =
      Ext_list.sublist
        all_elements (count_elements_before page) page.element_count
    in
    {elements;
     page_number = page.number;
     total_count = List.length all_elements}

  let map f content = {content with elements = List.map f content.elements}

  module Piqi = struct
    let to_personal_events {elements; page_number; total_count} =
      {Api_saisie_read_piqi.Paginated_personal_events.elements;
       page_number = Int32.of_int page_number;
       total_count = Int32.of_int total_count}

    let to_witnessed_events {elements; page_number; total_count} =
      {Api_saisie_read_piqi.Paginated_witnessed_events.elements;
       page_number = Int32.of_int page_number;
       total_count = Int32.of_int total_count}
  end
end
